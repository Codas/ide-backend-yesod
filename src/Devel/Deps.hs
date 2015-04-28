{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Devel.Deps (updatedDeps
                  ,checkCabalFile
                  ,getDeps
                  ,Deps
                  ) where

import           Control.Applicative ((<|>), many)
import           Control.Exception.Lifted (handle)
import           Control.Monad (unless, when, forM, forM_, filterM)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.State (StateT, get, put)
import           Control.Monad.Trans.Writer (WriterT, tell)
import           Control.Monad.Trans.Class (lift)
import qualified Data.Attoparsec.Text as A
import qualified Data.List                             as L
import qualified Data.ByteString as S
import qualified Data.Text as Text
import           Data.Char (isSpace, isUpper)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Control.Exception (SomeException, try, IOException)
import qualified System.FilePath as FP
import           System.Posix.Types                    (EpochTime)
import           System.PosixCompat.Files              (getFileStatus,
                                                        modificationTime, accessTime)
import           System.Directory
import           System.Exit (exitFailure)

import qualified Distribution.ModuleName               as D
import qualified Distribution.PackageDescription       as D
import qualified Distribution.PackageDescription.Parse as D
import qualified Distribution.Simple.Utils             as D
import qualified Distribution.Verbosity                as D

import           Text.Shakespeare (Deref)
import           Text.Julius      (juliusUsedIdentifiers)
import           Text.Cassius     (cassiusUsedIdentifiers)
import           Text.Lucius      (luciusUsedIdentifiers)

import Devel.CmdLine ()

updatedDeps :: Deps -> WriterT [String] (StateT (Map.Map String (Set.Set Deref)) IO) ()
updatedDeps = mapM_ go . Map.toList
  where
    go (x, (ys, ct)) = do
        isChanged <- handle (\(_ :: SomeException) -> return True) $ lift $
            case ct of
                AlwaysOutdated -> return True
                CompareUsedIdentifiers getDerefs -> do
                    derefMap <- get
                    ebs <- safeReadFile x
                    let newDerefs =
                            case ebs of
                                Left _ -> Set.empty
                                Right bs -> Set.fromList $ getDerefs
                                                         $ Text.unpack
                                                         $ Text.decodeUtf8With Text.lenientDecode bs
                    put $ Map.insert x newDerefs derefMap
                    case Map.lookup x derefMap of
                        Just oldDerefs | oldDerefs == newDerefs -> return False
                        _ -> return True
        when isChanged $ forM_ ys $ \y -> do
            n <- liftIO $ x `isNewerThan` y
            when n (tell [y])

checkCabalFile ::  IO ([String], D.Library)
checkCabalFile  =
  do cabal <- liftIO $ D.tryFindPackageDesc "."
     gpd <- liftIO $ D.readPackageDescription D.normal cabal
     case D.condLibrary gpd of
       Nothing -> failWith "incorrect cabal file, no library"
       Just ct ->
         case lookupDevelLib gpd ct of
           Nothing   ->
             failWith "no development flag found in your configuration file. Expected a 'library-only' flag or the older 'devel' flag"
           Just dLib -> do
              let hsSourceDirs = D.hsSourceDirs . D.libBuildInfo $ dLib
              fl <- getFileList hsSourceDirs []
              let unlisted = checkFileList fl dLib
              unless (null unlisted) $ do
                   putStrLn "WARNING: the following source files are not listed in exposed-modules or other-modules:"
                   mapM_ putStrLn unlisted
              when ("Application" `notElem` (map (last . D.components) $ D.exposedModules dLib)) $
                   putStrLn "WARNING: no exposed module Application"
              return (hsSourceDirs, dLib)



isNewerThan :: String -> String -> IO Bool
isNewerThan f1 f2 = do
  (_, mod1) <- getFileStatus' f1
  (_, mod2) <- getFileStatus' f2
  return (mod1 > mod2)

getFileStatus' :: String
               -> IO (System.Posix.Types.EpochTime, System.Posix.Types.EpochTime)
getFileStatus' fp = do
    efs <- try' $ getFileStatus fp
    case efs of
        Left _ -> return (0, 0)
        Right fs -> return (accessTime fs, modificationTime fs)

try' :: IO x -> IO (Either SomeException x)
try' = try

type FileList = Map.Map String EpochTime

getFileList :: [String] -> [String] -> IO FileList
getFileList hsSourceDirs extraFiles = do
    (files, deps) <- getDeps hsSourceDirs
    let files' = extraFiles ++ files ++ map fst (Map.toList deps)
    fmap Map.fromList $ forM files' $ \f -> do
        efs <- try $ getFileStatus f
        return $ case efs of
            Left (_ :: SomeException) -> (f, 0)
            Right fs -> (f, modificationTime fs)

allModules :: D.Library -> Set.Set String
allModules lib = Set.fromList $ map toString $ D.exposedModules lib ++ (D.otherModules . D.libBuildInfo) lib
    where
      toString = L.intercalate "." . D.components

lookupDevelLib :: D.GenericPackageDescription -> D.CondTree D.ConfVar c a -> Maybe a
lookupDevelLib gpd ct | found     = Just (D.condTreeData ct)
                      | otherwise = Nothing
  where
    flags = map (unFlagName . D.flagName) $ D.genPackageFlags gpd
    unFlagName (D.FlagName x) = x
    found = any (`elem` ["library-only", "devel"]) flags

checkFileList :: FileList -> D.Library -> [String]
checkFileList fl lib = filter (not . isSetup) . filter isUnlisted . filter isSrcFile $ sourceFiles
  where
    al = allModules lib
    -- a file is only a possible 'module file' if all path pieces start with a capital letter
    sourceFiles = filter isSrcFile . map fst . Map.toList $ fl
    isSrcFile file = let dirs = filter (/=".") $ FP.splitDirectories file
                     in  all (isUpper . head) dirs && (FP.takeExtension file `elem` [".hs", ".lhs"])
    isUnlisted file = not (toModuleName file `Set.member` al)
    toModuleName = L.intercalate "." . filter (/=".") . FP.splitDirectories . FP.dropExtension

    isSetup "Setup.hs" = True
    isSetup "./Setup.hs" = True
    isSetup "Setup.lhs" = True
    isSetup "./Setup.lhs" = True
    isSetup _ = False



type Deps = Map.Map String ([String], ComparisonType)

getDeps :: [String] -> IO ([String], Deps)
getDeps hsSourceDirs = do
    let defSrcDirs = case hsSourceDirs of
                        [] -> ["."]
                        ds -> ds
    hss <- fmap concat $ mapM findHaskellFiles defSrcDirs
    deps' <- mapM determineDeps hss
    return $ (hss, fixDeps $ zip hss deps')

fixDeps :: [(String, [(ComparisonType, String)])] -> Deps
fixDeps =
    Map.unionsWith combine . map go
  where
    go :: (String, [(ComparisonType, String)]) -> Deps
    go (x, ys) = Map.fromList $ map (\(ct, y) -> (y, ([x], ct))) ys

    combine (ys1, ct) (ys2, _) = (ys1 `mappend` ys2, ct)

findHaskellFiles :: String -> IO [String]
findHaskellFiles path = do
    contents <- getDirectoryContents path
    fmap concat $ mapM go contents
  where
    go ('.':_)          = return []
    go filename = do
        d <- doesDirectoryExist full
        if not d
          then if isHaskellFile
                  then return [full]
                  else return []
          else if isHaskellDir
                 then findHaskellFiles full
                 else return []
      where
        -- this could fail on unicode
        isHaskellDir  = isUpper (head filename)
        isHaskellFile = FP.takeExtension filename `elem` watch_files
        full = path FP.</> filename
        watch_files = [".hs", ".lhs"]


data TempType = StaticFiles String
              | Verbatim | Messages String | Hamlet | Widget | Julius | Cassius | Lucius
    deriving Show

safeReadFile :: MonadIO m => String -> m (Either IOException S.ByteString)
safeReadFile = liftIO . try . S.readFile

-- | How to tell if a file is outdated.
data ComparisonType = AlwaysOutdated
                    | CompareUsedIdentifiers (String -> [Deref])
    deriving Show

determineDeps :: String -> IO [(ComparisonType, String)]
determineDeps x = do
    y <- safeReadFile x
    case y of
        Left _ -> return []
        Right bs -> do
            let z = A.parseOnly (many (parser <|> (A.anyChar >> return Nothing)))
                  $ Text.decodeUtf8With Text.lenientDecode bs
            case z of
                Left _ -> return []
                Right r -> mapM go r >>= filterM (doesFileExist . snd) . concat
  where
    go (Just (StaticFiles fp, _)) = map ((,) AlwaysOutdated) <$> getFolderContents fp
    go (Just (Hamlet, f)) = return [(AlwaysOutdated, f)]
    go (Just (Widget, f)) = return
        [ (AlwaysOutdated, "templates/" ++ f ++ ".hamlet")
        , (CompareUsedIdentifiers $ map fst . juliusUsedIdentifiers, "templates/" ++ f ++ ".julius")
        , (CompareUsedIdentifiers $ map fst . luciusUsedIdentifiers, "templates/" ++ f ++ ".lucius")
        , (CompareUsedIdentifiers $ map fst . cassiusUsedIdentifiers, "templates/" ++ f ++ ".cassius")
        ]
    go (Just (Julius, f)) = return [(CompareUsedIdentifiers $ map fst . juliusUsedIdentifiers, f)]
    go (Just (Cassius, f)) = return [(CompareUsedIdentifiers $ map fst . cassiusUsedIdentifiers, f)]
    go (Just (Lucius, f)) = return [(CompareUsedIdentifiers $ map fst . luciusUsedIdentifiers, f)]
    go (Just (Verbatim, f)) = return [(AlwaysOutdated, f)]
    go (Just (Messages f, _)) = map ((,) AlwaysOutdated) <$> getFolderContents f
    go Nothing = return []

    parser = do
        ty <- (do _ <- A.string "\nstaticFiles \""
                  x' <- A.many1 $ A.satisfy (/= '"')
                  return $ StaticFiles x')
           <|> (A.string "$(parseRoutesFile " >> return Verbatim)
           <|> (A.string "$(hamletFile " >> return Hamlet)
           <|> (A.string "$(ihamletFile " >> return Hamlet)
           <|> (A.string "$(whamletFile " >> return Hamlet)
           <|> (A.string "$(html " >> return Hamlet)
           <|> (A.string "$(widgetFile " >> return Widget)
           <|> (A.string "$(Settings.hamletFile " >> return Hamlet)
           <|> (A.string "$(Settings.widgetFile " >> return Widget)
           <|> (A.string "$(juliusFile " >> return Julius)
           <|> (A.string "$(cassiusFile " >> return Cassius)
           <|> (A.string "$(luciusFile " >> return Lucius)
           <|> (A.string "$(persistFile " >> return Verbatim)
           <|> (
                   A.string "$(persistFileWith " >>
                   A.many1 (A.satisfy (/= '"')) >>
                   return Verbatim)
           <|> (do
                    _ <- A.string "\nmkMessage \""
                    A.skipWhile (/= '"')
                    _ <- A.string "\" \""
                    x' <- A.many1 $ A.satisfy (/= '"')
                    _ <- A.string "\" \""
                    _y <- A.many1 $ A.satisfy (/= '"')
                    _ <- A.string "\""
                    return $ Messages x')
        case ty of
            Messages{} -> return $ Just (ty, "")
            StaticFiles{} -> return $ Just (ty, "")
            _ -> do
                A.skipWhile isSpace
                _ <- A.char '"'
                y <- A.many1 $ A.satisfy (/= '"')
                _ <- A.char '"'
                A.skipWhile isSpace
                _ <- A.char ')'
                return $ Just (ty, y)

    getFolderContents :: String -> IO [String]
    getFolderContents fp = do
        cs <- getDirectoryContents fp
        let notHidden ('.':_) = False
            notHidden ('t':"mp") = False
            notHidden ('f':"ay") = False
            notHidden _ = True
        fmap concat $ forM (filter notHidden cs) $ \c -> do
            let f = fp ++ '/' : c
            isFile <- doesFileExist f
            if isFile then return [f] else getFolderContents f

failWith :: String -> IO a
failWith msg = do
    putStrLn $ "ERROR: " ++ msg
    exitFailure
