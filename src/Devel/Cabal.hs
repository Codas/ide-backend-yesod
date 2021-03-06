{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Devel.Cabal (startSession
                   ,LoadingStatus(..)
                   ,Error(..)
                   ,Span(..)
                   ,configure
                   ,toError
                   ) where

import           Control.Exception
import           Control.Arrow hiding (app)
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Loops (firstM)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as SB
import           Data.Char (isLetter, isDigit)
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid ((<>))
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Typeable (Typeable)
import           Distribution.Compiler
import           Distribution.InstalledPackageInfo (PError)
import           Distribution.ModuleName as Cabal
import qualified Distribution.ModuleName as ModuleName (fromString)
import           Distribution.Package
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parse
import           Distribution.PackageDescription.Configuration
import           Distribution.System
import           Distribution.Simple.Configure (getPersistBuildConfig)
import           Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
import qualified Distribution.ModuleName      as C
import qualified Distribution.Simple.Compiler as C
import           Distribution.Text (display)
import           Distribution.Version
import           Filesystem as FP
import "Glob"    System.FilePath.Glob (glob)
import           Filesystem.Loc as FL
import           Filesystem.Path.Find
import           Filesystem.Path (FilePath)
import qualified Filesystem.Path.CurrentOS as FP
import           IdeSession hiding (errorSpan,errorMsg)
import           Language.Haskell.Extension
import           Prelude hiding (FilePath,writeFile,pi)
import           System.Environment (getEnvironment)
import           System.Exit (ExitCode (..), exitFailure, exitSuccess)
import           System.Directory (getCurrentDirectory, doesFileExist)
import           System.FilePath ((</>), splitSearchPath)
import           System.Process (ProcessHandle,
                                 createProcess, env,
                                 getProcessExitCode,
                                 proc, readProcess,
                                 system, readProcessWithExitCode,
                                 terminateProcess)

import           Devel.Git

data Target =
  Target {targetFiles :: !(Set (Loc 'Relative 'File))
         ,targetExtensions :: ![Extension]
         ,targetDependencies :: ![PackageName]}
  deriving (Show)

-- | Source span.
data Span =
  Span {spanSL :: !Int
       ,spanSC :: !Int
       ,spanEL :: !Int
       ,spanEC :: !Int}
  deriving (Show,Eq)

-- | Some error message.
data Error =
  Error {errorFile :: !Text
        ,errorSpan :: !Span
        ,errorMsg :: !Text}
  deriving (Eq,Show)

-- | Status for loading the project.
data LoadingStatus
  = NotLoading
  | Loading !Int !Int !Text
  | LoadOK ![Text]
  | LoadFailed ![Either Text Error]
  deriving (Eq,Show)

-- | All exceptions thrown by the library.
data FPException
  = FPNoCabalFile FilePath
  | FPInvalidCabalFile (Loc 'Absolute 'File) PError
  deriving (Show,Typeable)
instance Exception FPException

-- | Get the .cabal filename from the given package directory.
getCabalFp :: Loc 'Absolute 'Dir -> IO (Loc 'Absolute 'File)
getCabalFp pkgDir =
  do mcabal <- findFileUp pkgDir
                          (flip FP.hasExtension "cabal" . FL.toFilePath)
                          (Just pkgDir)
     case mcabal of
       Nothing -> throwIO (FPNoCabalFile (FL.toFilePath pkgDir))
       Just cabalfp -> return cabalfp

getCabalFile :: IO String
getCabalFile = do
  list <- glob "*cabal"
  case list of
    [] -> fail "No cabal file."
    (cabalFile:_) -> return cabalFile

-- TODO: fix hardcoded sources...
-- stackDir :: FilePath
-- stackDir = "dist-stack" FP.</> "x86_64-osx" FP.</> "Cabal-1.22.2.0"

-- stackBuildDir :: FilePath
-- stackBuildDir = stackDir FP.</> "build"

-- stackAutogenDir :: FilePath
-- stackAutogenDir = stackBuildDir FP.</> "autogen"

-- | Start the session with the cabal file, will not proceed if the
-- targets are unambiguous, in which case it will be continued later
-- after prompting the user.
startSession :: TChan LoadingStatus
             -> IO (IdeSession, IdeSessionUpdate -> IO (Maybe [Either Text Error]))
startSession loading =
  do dir <- getWorkingDir
     dirStr <- getCurrentDirectory
     cabalfp <- getCabalFp dir
     target <- getTarget cabalfp
     (extensionList, srcDir, cabalSrcList, packageName) <- getExtensions
     packages <- getPackages packageName
     void (configure packageName)
     let flags = packages ++ extensionList ++ opts target
     sessionConfig <- sessionConfigFromEnv
     session <-
       initSession (sessionParams flags dirStr)
                   sessionConfig {configGenerateModInfo = False
                                 ,configLocalWorkingDir = Just dirStr}
     let reloadSession extra = loadFiles session target S.empty extra loading
     return (session, reloadSession)
  where sessionParams flags dirStr =
          defaultSessionInitParams {sessionInitGhcOptions = ["-w", "-dynamic", "-O0", "-hide-all-packages"] <> flags
                                   ,sessionInitTargets = TargetsInclude [dirStr </> "Application.hs"]}
        opts target = map showExt (targetExtensions target) <> ["-optP-DDEVELOPMENT"]
        showExt :: Extension -> String
        showExt (EnableExtension e) = "-X" <> show e
        showExt (DisableExtension e) = "-XNo" <> show e
        showExt (UnknownExtension e) = "-X" <> show e


getPackages :: String -> IO [String]
getPackages pkg = do
  (_, stdout, _) <- readProcessWithExitCode "stack" ["list-dependencies"] ""
  let packageList :: [String]
      packageList = concatMap (\s -> ["-package", s]) $ filter (/= pkg) $ map (takeWhile (/= ' ')) $ lines stdout
  return ("-hide-all-packages" : packageList)

-- | Parse the cabal file to get the ghc extensions in use.
getExtensions :: IO ([String], [String], [String], String)
getExtensions = do
  cabalFilePath <- getCabalFile
  cabalFile <- Prelude.readFile cabalFilePath

  let unsafePackageDescription = parsePackageDescription cabalFile

      genericPackageDescription = case unsafePackageDescription of
                                ParseOk _ a -> a
                                _           -> error "failed package description."

      packDescription = flattenPackageDescription genericPackageDescription

      packName = unPackageName (pkgName (package packDescription))

      rawExt = usedExtensions $ head $ allBuildInfo packDescription

      lib = fromMaybe emptyLibrary $ library packDescription

      -- I think it would be wise to avoid src files under executable to avoid conflict.
      srcDir  = hsSourceDirs $ libBuildInfo lib
      srcList = extraSrcFiles packDescription


      parseExtension :: Extension -> String
      parseExtension (EnableExtension extension) =  "-X" ++ show extension
      parseExtension (DisableExtension extension) = "-XNo" ++ show extension
      parseExtension (UnknownExtension extension) = "-X" ++ show extension

      extensions = map parseExtension rawExt

      -- Now we handle source files from executable section here.
      execList = executables packDescription

  paths <- mapM getPathList execList

  return (extensions, srcDir, srcList ++ concat paths, packName)
  where
    getPathList :: Executable -> IO [String]
    getPathList exec =
      let mainModule' = modulePath exec
          bInfo = buildInfo exec
          execModuleList = otherModules bInfo
          srcDirsList = hsSourceDirs bInfo
          execSrcFileList = map C.toFilePath execModuleList
          nonPaths = [dir </> fp | fp <- execSrcFileList, dir <- srcDirsList]
          paths' = map (</> mainModule' ) srcDirsList -- For the main module
                   ++ [x++y | x <- nonPaths, y <- [".hs", ".lhs"]]

      in filterM doesFileExist paths'


configure :: String -> IO Bool
configure pkgName =
  do env <- getEnvironment
     status <- createProcess (proc "stack"
                                [ "build"
                                , "--only-configure"
                                , "--flag"
                                , pkgName ++ ":-library-only"
                                , "--flag"
                                , pkgName ++ ":-dev"
                                , "--no-test"
                                , "--no-bench"
                                , "--no-library-profiling"
                                ])
     checkExit status

-- | Get library target of a package.
getTarget :: Loc 'Absolute 'File -> IO Target
getTarget cabalfp =
  do chars <- Prelude.readFile (FL.encodeString cabalfp)
     case parsePackageDescription chars of
       ParseFailed per ->
         throwIO (FPInvalidCabalFile cabalfp per)
       ParseOk _ (resolvePackage -> pkg) ->
         do let dir = FL.parentOfFile cabalfp
                pname = pkgName (package pkg)
            Just (lib,(exts,files)) <- maybe (return Nothing)
                                            (\e -> (fmap (Just . (e,)) .
                                                    libraryFiles pname dir) e)
                                            (library pkg)
            return (Target files exts (deps (libBuildInfo lib)))
         where deps = map dependencyName . targetBuildDepends
               dependencyName (Dependency name _) = name

-- | Get all files referenced by the library.
libraryFiles :: PackageName
             -> Loc 'Absolute 'Dir
             -> Library
             -> IO ([Extension],Set (Loc 'Relative 'File))
libraryFiles pname dir lib =
  do exposed <- resolveFiles
                  (map (either (error . show) id .
                        FL.parseRelativeDirLoc . FP.decodeString)
                       (defaultSrcDirs ++ hsSourceDirs build))
                  (map Left (exposedModules lib) ++
                   [paths_Name pname])
                  haskellFileExts
     (es,bfiles) <- buildFiles pname dir Nothing build
     return (es,mconcat [bfiles,exposed])
  where build = libBuildInfo lib

haskellFileExts :: [Text]
haskellFileExts = ["hs","hsc","lhs"]

-- | Default src dirs for Cabal targets.
defaultSrcDirs :: [String]
defaultSrcDirs = ["", FP.encodeString ("dist" FP.</> "build" FP.</> "autogen")]

-- | Try to resolve the list of base names in the given directory by
-- looking for unique instances of base names applied with the given
-- extensions.
resolveFiles :: [Loc 'Relative 'Dir] -- ^ Directories to look in.
             -> [Either Cabal.ModuleName String] -- ^ Base names.
             -> [Text] -- ^ Extentions.
             -> IO (Set (Loc 'Relative 'File))
resolveFiles dirs names exts =
  fmap (S.fromList . catMaybes) (forM names makeNameCandidates)
  where makeNameCandidates name =
          firstM (isFile . FL.toFilePath)
                 (concatMap (makeDirCandidates name) dirs)
        makeDirCandidates :: Either Cabal.ModuleName String
                          -> Loc 'Relative 'Dir
                          -> [Loc 'Relative 'File]
        makeDirCandidates name dir =
          map (\ext ->
                 case name of
                   Left mn ->
                     (either (error . show)
                             (FL.appendLoc dir)
                             (FL.parseRelativeFileLoc
                                (FP.addExtension (FP.decodeString (Cabal.toFilePath mn))
                                                 ext)))
                   Right fp ->
                     either (error . show)
                            (FL.appendLoc dir)
                            (FL.parseRelativeFileLoc (FP.decodeString fp)))
              exts


-- | Get all files in a build. If any of this target's ('BuildInfo')
-- dependencies are the same as the package name, then this means it's
-- a target which depends on the library component, in which case the
-- necessary files and extensions should be drawn from the library,
-- too.
buildFiles :: PackageName
           -> Loc 'Absolute 'Dir
           -> Maybe Library
           -> BuildInfo
           -> IO ([Extension],Set (Loc 'Relative 'File))
buildFiles pname dir mlib build =
  do other <- resolveFiles
                (map (either (error . show) id .
                      FL.parseRelativeDirLoc . FP.decodeString)
                     (defaultSrcDirs ++ hsSourceDirs build))
                (map Left (otherModules build) ++
                 [paths_Name pname])
                haskellFileExts
     let (exts,files) =
           (defaultExtensions build ++ oldExtensions build ++
                                       otherExtensions build
           ,mconcat [other
                    ,S.fromList
                       (map (either (error . show) id .
                             FL.parseRelativeFileLoc . FP.decodeString)
                            (cSources build))])
     case mlib of
       Just lib
         | elem pname (map dependencyName (targetBuildDepends build)) ->
           do (libExts,libFiles) <- libraryFiles pname dir lib
              return (exts ++ libExts,mappend files libFiles)
       _ -> return (exts,files)
  where dependencyName (Dependency name _) = name


-- | Get all dependencies of a package, including library,
-- executables, tests, benchmarks.
resolvePackage  :: GenericPackageDescription -> PackageDescription
resolvePackage (GenericPackageDescription desc defaultFlags mlib _ _ _) =
  desc {library = fmap (resolveConditions flags' updateLibDeps . ("",)) mlib}
  where flags = M.union mempty (flagMap defaultFlags)
        flags' =
          map ((FlagName . T.unpack) . fst) (filter snd (M.toList flags))
        updateLibDeps lib _ deps =
          lib {libBuildInfo =
                 (libBuildInfo lib) {targetBuildDepends = deps}}

-- | Resolve the condition tree for the library.
resolveConditions :: (Monoid target,HasName target)
                  => [FlagName]
                  -> (target -> String -> cs -> target)
                  -> (String,CondTree ConfVar cs target)
                  -> target
resolveConditions flags addDeps (name,CondNode lib deps cs) =
  appendTargets basic children
  where basic = addDeps lib name deps
        children =
          foldr (appendTargets . apply) mempty cs
          where apply (cond,node,mcs) =
                  if condSatisfied cond
                     then appendTargets
                            (resolveConditions flags
                                               addDeps
                                               (name,node))
                            (maybe mempty
                                   (resolveConditions flags addDeps .
                                    (name,))
                                   mcs)
                     else mempty
                condSatisfied c =
                  case c of
                    Var v -> varSatisifed v
                    Lit b -> b
                    CNot c' ->
                      not (condSatisfied c')
                    COr cx cy ->
                      condSatisfied cx || condSatisfied cy
                    CAnd cx cy ->
                      condSatisfied cx && condSatisfied cy
                varSatisifed v =
                  case v of
                    OS os -> os == buildOS
                    Arch arch -> arch == buildArch
                    Flag flag -> flag `elem` flags
                    Impl flavor range ->
                      case buildCompilerId of
                        CompilerId flavor' ver ->
                          flavor' == flavor &&
                          withinRange ver range

-- | Safely append two targets without throwing an exception.
-- See here for explanation: https://github.com/haskell/cabal/blob/81330d032a174e8406bcd40e9c5c8c8cbdd6853b/Cabal/Distribution/PackageDescription.hs#L566..L570
appendTargets :: (HasName m,Monoid m) => m -> m -> m
appendTargets x y =
  if not (null (getName x)) &&
     not (null (getName y))
     then fst (modifyName (const "") x) <> y
     else x <> y

-- | Get the name.
getName :: HasName m => m -> String
getName = snd . modifyName id

-- | Some target that has a name.
class HasName m where
  modifyName :: (String -> String) -> m -> (m,String)

instance HasName Library where
    modifyName f m = (m,f "")
instance HasName Executable where
    modifyName f (Executable name m b) =
        (Executable (f name) m b,f name)
instance HasName TestSuite where
    modifyName f (TestSuite name i b e) =
        (TestSuite (f name) i b e, f name)
instance HasName Benchmark where
    modifyName f (Benchmark name i b e) =
        (Benchmark (f name) i b e, f name)


-- | Make a map from a list of flag specifications.
--
-- What is @flagManual@ for?
flagMap :: [Flag] -> Map Text Bool
flagMap = M.fromList . map pair
  where pair :: Flag -> (Text, Bool)
        pair (MkFlag (unName -> name) _desc def _manual) = (name,def)
        unName (FlagName t) = T.pack t


-- | Load the package files and update the app state of the progress.
loadFiles :: IdeSession
          -> Target
          -> Set FilePath
          -> IdeSessionUpdate
          -> TChan LoadingStatus
          -> IO (Maybe [Either Text Error])
loadFiles sess target files extra loading =
  do atomically (writeTChan loading NotLoading)
     updateSession
       sess
       (updateCodeGeneration True)
       (\updateStatus ->
          atomically
            (writeTChan loading (handleUpdateStatus updateStatus)))
     errs <- fmap (filter isError)
                  (getSourceErrors sess)
     putStrLn  "Updates done"
     if null errs
        then do atomically (writeTChan loading (LoadOK (map (T.pack . FL.encodeString) (sort loadedFiles))))
                return Nothing
        else do atomically (writeTChan loading (LoadFailed (map toError errs)))
                return (Just (map toError errs))
  where handleUpdateStatus (UpdateStatusProgress progress) =
          (Loading (progressStep progress)
                   (progressNumSteps progress)
                   (fromMaybe (fromMaybe "Unknown step" (progressOrigMsg progress))
                              (progressParsedMsg progress)))
        handleUpdateStatus UpdateStatusDone            = LoadOK ["All Done"]
        handleUpdateStatus UpdateStatusFailedToRestart = LoadFailed [Left "Server died, failed to restart"]
        handleUpdateStatus (UpdateStatusFailed t)      = LoadFailed [Left t]
        handleUpdateStatus (UpdateStatusErrorRestart t)= LoadFailed [Left t]
        handleUpdateStatus UpdateStatusRequiredRestart = LoadFailed [Left "Restart Required"]
        loadedFiles = S.toList (targetFiles target)
        justStripPrefix p s = fromMaybe s (stripPrefix p s)
        isError (SourceError{errorKind = k}) =
          case k of
            KindError -> True
            KindServerDied -> True
            KindWarning -> False

-- | Convert a source error to either an unhelpful text span or a
-- proper span.
toError :: SourceError -> Either Text Error
toError (SourceError _ espan msg) =
  case espan of
    ProperSpan (SourceSpan path sl sc el ec) ->
      Right Error {errorFile = T.pack path
                  ,errorSpan = Span sl sc el ec
                  ,errorMsg = msg}
    TextSpan e -> Left e


-- | Make a file resolution entry for the special Paths_projectname module.
paths_Name :: PackageName -> Either Cabal.ModuleName a
paths_Name pname = Left (ModuleName.fromString ("Paths_" <> map normalize (display pname)))
  where normalize c | isLetter c || isDigit c = c
                    | otherwise = '_'


-- | nonblocking version of @waitForProcess@
waitForProcess' :: ProcessHandle -> IO ExitCode
waitForProcess' pid = go
  where
    go = do
      mec <- getProcessExitCode pid
      case mec of
        Just ec -> return ec
        Nothing -> threadDelay 100000 >> go

-- | wait for process started by @createProcess@, return True for ExitSuccess
checkExit :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO Bool
checkExit (_,_,_,h) = (==ExitSuccess) <$> waitForProcess' h
