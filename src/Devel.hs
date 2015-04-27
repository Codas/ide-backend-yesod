{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts  #-}
module Main where

import qualified Distribution.ModuleName               as D
import qualified Distribution.PackageDescription       as D
import qualified Distribution.PackageDescription.Parse as D
import qualified Distribution.Simple.Utils             as D
import qualified Distribution.Verbosity                as D

import           Control.Applicative ((<|>), many)
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (async)
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TMVar
import qualified Control.Exception as Ex
import           Control.Exception.Lifted (handle)
import           Control.Monad (forever, unless, void, when, forM, forM_, filterM)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.State (StateT, get, put, runStateT)
import           Control.Monad.Trans.Writer (WriterT, tell, execWriterT)
import           Control.Monad.Trans.Class (lift)
import qualified Data.Attoparsec.Text as A
import qualified Data.ByteString.Lazy as LB
import qualified Data.List                             as L
import qualified Data.ByteString as S
import           Data.Char (isSpace, isUpper)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Default.Class (def)
import           Data.FileEmbed (embedFile)
import           Data.Function
import qualified Data.IORef as I
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Streaming.Network (bindPortTCP)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import qualified Data.Text.IO as Text
import           Filesystem.Path.CurrentOS (toText, filename, extension)
import           Filesystem.Path (FilePath, )
import qualified Filesystem as FS
import           Network (withSocketsDo)
import           Network.HTTP.Client (managerSetProxy, noProxy)
import           Network.HTTP.Conduit (conduitManagerSettings, newManager)
import           Network.HTTP.ReverseProxy             (ProxyDest (ProxyDest),
                                                        waiProxyToSettings, wpsTimeout, wpsOnExc)
import qualified Network.HTTP.ReverseProxy as ReverseProxy
import           Network.HTTP.Types (status200, status503)
import           Network.Socket (sClose)
import           Network.Wai (responseLBS, requestHeaders)
import           Network.Wai.Handler.Warp (run, defaultSettings, setPort)
import           Network.Wai.Handler.WarpTLS (runTLS, tlsSettingsMemory)
import           Network.Wai.Parse (parseHttpAccept)
import           Prelude hiding (mod, span, FilePath)
import qualified System.FilePath as FP
import           SrcLoc (Located)
import           System.Directory
import           System.Exit ( ExitCode (..)
                             , exitFailure
                             , exitSuccess)
import           System.FSNotify
import           System.Posix.Types                    (EpochTime)
import           System.PosixCompat.Files              (getFileStatus,
                                                        modificationTime, accessTime)
import           IdeSession hiding (updateSession, initSession)
import           Devel.Cabal
import           Devel.CmdLine

import           Text.Shakespeare (Deref)
import           Text.Julius      (juliusUsedIdentifiers)
import           Text.Cassius     (cassiusUsedIdentifiers)
import           Text.Lucius      (luciusUsedIdentifiers)

type UpdateSession = IdeSessionUpdate -> IO (Maybe [Either Text Error])
type Runner = Maybe (RunActions RunResult)

data FileChangeType = FileAdded | FileModified | FileRemoved
    deriving (Show, Eq)

data FileType = HaskellFile | DataFile | CabalFile
    deriving (Show, Eq)

data FileChange = FileChange
                  {fileChangePath :: String
                  ,fileChangeType :: FileChangeType
                  ,fileType       :: FileType}
    deriving (Show, Eq)

data RunCommand = Start | Stop

watchThread :: TChan FileChange -> IO ()
watchThread writeChan = withManager (\mgr ->
  do dir <- fmap (either id id . toText) FS.getWorkingDirectory
     _ <- watchTree mgr                 -- manager
                    "."                 -- directory to watch
                    (shouldReload dir)  -- predicate
                    (atomically . writeTChan writeChan . toFileChange)
     -- sleep forever (until interrupted)
     forever $ threadDelay (1000 * 1000 * 10))
  where toFileChange event =
          let stringPath = fpToString filePath
              filePath = eventPath event
              fileType = whichFileType filePath
              fileChangeType =
                case event of
                  Added _ _    -> FileAdded
                  Removed _ _  -> FileRemoved
                  Modified _ _ -> FileModified
          in FileChange stringPath fileChangeType fileType
        whichFileType filePath
          | isHsFile    filePath = HaskellFile
          | isCabalFile filePath = CabalFile
          | otherwise            = DataFile
        isHsFile :: FilePath -> Bool
        isHsFile fp = any (`elem` extension fp) haskellFileExts
        isCabalFile fp = filename fp == "cabal.sandbox.config" || ".cabal" `elem` extension fp
        haskellFileExts :: [Text]
        haskellFileExts = ["hs","hsc","lhs"]
        fpToString = Text.unpack . either id id . toText

shouldReload :: Text -> Event -> Bool
shouldReload dir event = not (or conditions)
  where fp = case event of
              Added filePath _ -> filePath
              Modified filePath _ -> filePath
              Removed filePath _ -> filePath
        p = case toText fp of
              Left filePath -> filePath
              Right filePath -> filePath
        fn = case toText (filename fp) of
                Left filePath -> filePath
                Right filePath -> filePath
        conditions = [ notInPath ".git", notInPath "yesod-devel", notInPath "dist"
                     , notInPath "session.", notInFile ".tmp"
                     , notInFile "#", notInPath ".cabal-sandbox", notInFile "flycheck_"]
        notInPath t = t `Text.isInfixOf` stripPrefix dir p
        notInFile t = t `Text.isInfixOf` fn
        stripPrefix pre t = fromMaybe t (Text.stripPrefix pre t)

handleStatusUpdates :: TChan LoadingStatus -> IO ()
handleStatusUpdates loading' =
  do loading <- atomically (dupTChan loading')
     forever $
       do status <- atomically $ readTChan loading
          case status of
            NotLoading -> Text.putStrLn "Currently not loading"
            LoadOK _ -> Text.putStrLn "Loading complete"
            LoadFailed msgs -> mapM_ (either Text.putStrLn printError) msgs
              where printError Error{..} =
                      do Text.putStrLn errorMsg
                         let locS = Text.pack ("(" <> show (spanSL errorSpan) <> "," <> show (spanSC errorSpan) <> ")")
                             locE = Text.pack ("(" <> show (spanEL errorSpan) <> "," <> show (spanEC errorSpan) <> ")")
                         Text.putStrLn $ "    Occured in " <> errorFile <> ":" <> locS <> "-" <> locE
            Loading step steps msg ->
              do let stepReport = mconcat ["[", show step, " of ", show steps, "] "]
                 Text.putStrLn (Text.pack stepReport <> msg)

handleFilesChanged :: TChan FileChange
                   -> TVar Deps
                   -> TVar IdeSession
                   -> TMVar RunCommand
                   -> TVar UpdateSession
                   -> TChan LoadingStatus
                   -> IO ()
handleFilesChanged filesChanged' depsTVar sessionTVar runCommandTMVar updateSessionTVar loading =
  do filesChanged <- atomically (dupTChan filesChanged')
     lastChangedFileRef <- I.newIORef Nothing
     forever $
       do (FileChange filePath fileChangeType fileType) <- atomically $ readTChan filesChanged
          lastChangedFile <- I.readIORef lastChangedFileRef
          content <- LB.readFile filePath
          I.writeIORef lastChangedFileRef (Just (filePath, content))
          unless (lastChangedFile == Just (filePath, content)) $
            do deps <- atomically (readTVar depsTVar)
               let changes = runStateT (execWriterT (updatedDeps deps))
               (depHsFiles, _) <- changes mempty
               let extra = mconcat (getExtra depHsFiles)
               case (fileChangeType, fileType) of
                 (FileAdded,    HaskellFile) -> updateFile (updateSourceFileFromFile filePath <> extra)
                 (FileAdded,    DataFile)    -> updateFile (updateDataFile           filePath content <> extra)
                 (FileModified, HaskellFile) -> updateFile (updateSourceFileFromFile filePath <> extra)
                 (FileModified, DataFile)    -> updateFile (updateDataFile           filePath content <> extra)
                 (FileRemoved,  HaskellFile) -> updateFile (updateSourceFileDelete   filePath)
                 (FileRemoved,  DataFile)    -> updateFile (updateDataFileDelete     filePath)
                 (_,            CabalFile)   ->
                   do atomically (putTMVar runCommandTMVar Stop)
                      session <- atomically (readTVar sessionTVar)
                      shutdownSession session
                      (newSession, newUpdateSession, newDeps) <- initSession loading
                      atomically $ do writeTVar sessionTVar newSession
                                      writeTVar updateSessionTVar newUpdateSession
                                      writeTVar depsTVar newDeps
                      atomically (putTMVar runCommandTMVar Start)
  where getExtra = map (updateSourceFileDelete <> updateSourceFileFromFile)
        updateFile :: IdeSessionUpdate -> IO ()
        updateFile upd =
          do develHsPath <- checkDevelFile
             atomically (putTMVar runCommandTMVar Stop)
             updateSession <- atomically (readTVar updateSessionTVar)
             void (updateSession (upd <> updateSourceFileFromFile develHsPath))
             atomically (putTMVar runCommandTMVar Start)

main :: IO ()
main = do
  opts@Options{..} <- getCommandLineOptions
  loading <- newTChanIO
  filesChanged <- newTChanIO
  (session, updateSession, deps) <- initSession loading
  depsTVar <- newTVarIO deps
  updateSessionTVar <- newTVarIO updateSession
  sessionTVar <- newTVarIO session
  runnerTVar <- newTVarIO Nothing
  runCommandTMVar <- newEmptyTMVarIO
  atomically (putTMVar runCommandTMVar Start)
  Ex.bracket_
     (do _ <- async (runDevel opts sessionTVar runnerTVar runCommandTMVar)
         _ <- async (handleStatusUpdates loading)
         _ <- async (watchThread filesChanged)
         _ <- async (handleFilesChanged filesChanged depsTVar sessionTVar runCommandTMVar updateSessionTVar loading)
         return ())
     (threadDelay (1000 * 1000 * 60 * 60 * 24 * 365)) -- run devel server for up to one year
     (do runner <- atomically (readTVar runnerTVar)
         putStrLn "Shutting down the runner"
         mapM_ interrupt runner
         exitSuccess)
  exitSuccess

initSession :: TChan LoadingStatus -> IO (IdeSession, UpdateSession, Deps)
initSession loading =
  do (session, updateSession) <- startSession loading
     (hsSourceDirs, _) <- checkCabalFile
     (_, deps) <- getDeps hsSourceDirs
     develHsPath <- checkDevelFile
     content <- LB.readFile develHsPath
     void (updateSession (updateSourceFile develHsPath content))
     return (session, updateSession, deps)

runLoop :: RunActions RunResult -> IO ()
runLoop ra =
  let loop = do x <- runWait ra
                case x  of
                  Left status -> Text.putStr (Text.decodeUtf8 status) >> loop
                  Right RunOk -> putStrLn "Application finished running."
                  Right (RunProgException ex) -> putStrLn ex
                  Right (RunGhcException ex) -> putStrLn ex
                  Right RunForceCancelled -> putStrLn "Force shutdown occured."
                  Right RunBreak -> putStrLn "Halting on Breakpoint."
   in loop

runDevel :: Options
         -> TVar IdeSession
         -> TVar Runner
         -> TMVar RunCommand
         -> IO ()
runDevel opts sessionTVar runnerTVar runCommandTMVar =
  forever $
    do cmd <- atomically (takeTMVar runCommandTMVar)
       case cmd of
          Start ->
            do session <- atomically (readTVar sessionTVar)
               ra <- runStmt session "Main" "main"
               atomically (writeTVar runnerTVar (Just ra))
               void (async (runLoop ra))
               return ()
          Stop ->
            do runner <- atomically $ do runner <- readTVar runnerTVar
                                         writeTVar runnerTVar Nothing
                                         return runner
               mapM_ interrupt runner
               return ()




checkDevelFile :: IO String
checkDevelFile =
    loop paths
  where
    paths = ["app/devel.hs", "devel.hs", "src/devel.hs"]
    loop [] = failWith $ "file devel.hs not found, checked: " ++ show paths
    loop (x:xs) = do
        e <- doesFileExist x
        if e
            then return x
            else loop xs

failWith :: String -> IO a
failWith msg = do
    putStrLn $ "ERROR: " ++ msg
    exitFailure




updatedDeps :: Deps -> WriterT [String] (StateT (Map.Map String (Set.Set Deref)) IO) ()
updatedDeps = mapM_ go . Map.toList
  where
    go (x, (ys, ct)) = do
        isChanged <- handle (\(_ :: Ex.SomeException) -> return True) $ lift $
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

try' :: IO x -> IO (Either Ex.SomeException x)
try' = Ex.try

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

type FileList = Map.Map String EpochTime

getFileList :: [String] -> [String] -> IO FileList
getFileList hsSourceDirs extraFiles = do
    (files, deps) <- getDeps hsSourceDirs
    let files' = extraFiles ++ files ++ map fst (Map.toList deps)
    fmap Map.fromList $ forM files' $ \f -> do
        efs <- Ex.try $ getFileStatus f
        return $ case efs of
            Left (_ :: Ex.SomeException) -> (f, 0)
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

safeReadFile :: MonadIO m => String -> m (Either Ex.IOException S.ByteString)
safeReadFile = liftIO . Ex.try . S.readFile

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
            let z = A.parseOnly (many $ (parser <|> (A.anyChar >> return Nothing)))
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
