{-# LANGUAGE OverloadedStrings  #-}

module Main where

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
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as SB
import           Data.Default.Class (def)
import           Data.FileEmbed (embedFile)
import           Data.Function
import qualified Data.IORef as I
import           Data.Maybe
import           Data.Monoid
import           Data.List (foldl', intersperse)
import qualified Data.Set as Set
import           Data.Streaming.Network (bindPortTCP)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import qualified Data.Text.IO as Text
import           Filesystem.Path.CurrentOS (fromText, toText, filename, extension)
import           Filesystem.Path (FilePath)
import qualified Filesystem as FS
import           Network (withSocketsDo)
import           Network.HTTP.Client (newManager, defaultManagerSettings, managerSetProxy, noProxy)
import           Network.HTTP.Conduit (conduitManagerSettings, newManager)
import           Network.HTTP.ReverseProxy (WaiProxyResponse (WPRProxyDest)
                                           ,ProxyDest (ProxyDest), waiProxyTo, wpsTimeout
                                           ,waiProxyToSettings, defaultOnExc, wpsOnExc)
import           Network.HTTP.Types (status200, status502)
import           Network.Socket (sClose)
import           Network.Wai (Application, responseLBS, requestHeaders)
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
import           System.Environment (setEnv)
import           System.PosixCompat.Files (touchFile)
import           IdeSession hiding (initSession, updateDataFile, updateDataFileDelete
                                   ,updateSourceFile, updateSourceFileFromFile
                                   ,updateSourceFileDelete)

import           Devel.Cabal
import           Devel.CmdLine
import           Devel.Deps

type UpdateSessionFn = IdeSessionUpdate -> IO (Maybe [Either Text Error])
type Runner = Maybe (RunActions RunResult)

data FileChangeType = FileAdded | FileModified | FileRemoved
    deriving (Show, Eq)

data FileType = HaskellFile | DataFile | StaticFile | CabalFile
    deriving (Show, Eq)

data FileChange = FileChange
                  {fileChangePath :: String
                  ,fileChangeType :: FileChangeType
                  ,fileType       :: FileType}
    deriving (Show, Eq)

data RunCommand = Start | Stop

watchThread :: TChan FileChange -> IO ()
watchThread writeChan = withManager (\mgr ->
  do dir <- fmap fpToText FS.getWorkingDirectory
     let isStaticFile fp = "/static/" `Text.isPrefixOf` stripPrefix dir (Text.pack (FP.takeDirectory fp))
         toFileChange event =
           let fpToString = Text.unpack . stripPrefix (dir <> "/") . either id id . toText
               filePath = eventPath event
               fileType = whichFileType filePath
               fileChangeType =
                 case event of
                   Added _ _    -> FileAdded
                   Removed _ _  -> FileRemoved
                   Modified _ _ -> FileModified
           in FileChange filePath fileChangeType fileType
         whichFileType filePath
           | isHsFile     filePath = HaskellFile
           | isCabalFile  filePath = CabalFile
           | isStaticFile filePath = StaticFile
           | otherwise             = DataFile
     _ <- watchTree mgr                 -- manager
                    "."                 -- directory to watch
                    (shouldReload dir)  -- predicate
                    (atomically . writeTChan writeChan . toFileChange)
     -- sleep forever (until interrupted)
     forever $ threadDelay (1000 * 1000 * 10))
  where mgrConfig = defaultConfig {confDebounce = Debounce 2}
        isHsFile :: FP.FilePath -> Bool
        isHsFile fp = FP.takeExtension fp `elem` haskellFileExts
        isCabalFile fp = FP.takeFileName fp == "cabal.sandbox.config" || ".cabal" == FP.takeExtension fp
        haskellFileExts :: [String]
        haskellFileExts = [".hs",".hsc",".lhs"]
        fpToText = either id id . toText
        stripPrefix pre t = fromMaybe t (Text.stripPrefix pre t)

shouldReload :: Text -> Event -> Bool
shouldReload dir event = not (or conditions)
  where fp = case event of
              Added filePath _ -> filePath
              Modified filePath _ -> filePath
              Removed filePath _ -> filePath
        p = Text.pack fp
        fn = Text.pack (FP.takeFileName fp)
        modified = case event of
                     Modified _ _ -> True
                     _        -> False
        conditions = [ inPath ".git", inPath "yesod-devel", inPath "dist"
                     , inPath "rpc.", inPath "/upload/"
                     , inPath "session.", inFile ".tmp", inPath "scss"
                     , inFile "#", inPath ".cabal-sandbox", inFile "flycheck_"
                     , fileStartsWith "."]
        inPath t = t `Text.isInfixOf` stripPrefix dir p
        inFile t = t `Text.isInfixOf` fn
        fileStartsWith t = t `Text.isPrefixOf` fn
        stripPrefix pre t = fromMaybe t (Text.stripPrefix pre t)

handleStatusUpdates :: TChan LoadingStatus -> TVar Text -> IO ()
handleStatusUpdates loading' loadingText =
  do loading <- atomically (dupTChan loading')
     forever $
       do status <- atomically $ readTChan loading
          case status of
            NotLoading -> setLoadingText "Currently not loading"
            LoadOK _ -> setLoadingText "Loading complete"
            LoadFailed msgs -> setLoadingText $ foldl' mappend "" $ intersperse "\n\n" $ map (either id errorToText) msgs
              where errorToText Error{..} =
                      let locS = Text.pack ("(" <> show (spanSL errorSpan) <> "," <> show (spanSC errorSpan) <> ")")
                          locE = Text.pack ("(" <> show (spanEL errorSpan) <> "," <> show (spanEC errorSpan) <> ")")
                      in errorMsg <> "\n    Occured in " <> errorFile <> ":" <> locS <> "-" <> locE
            Loading step steps msg ->
              do let stepReport = mconcat ["[", show step, " of ", show steps, "] "]
                 appendLoadingText (Text.pack stepReport <> msg)
  where setLoadingText t = do Text.putStrLn t
                              atomically $ writeTVar loadingText t
        appendLoadingText t = do Text.putStrLn t
                                 atomically $ modifyTVar loadingText ((t <> "\n") `Text.append`)

updateDataFileDelete _ = updateCodeGeneration True

updateDataFile _ _ = updateCodeGeneration True

updateSourceFileDelete _ = updateCodeGeneration True

updateSourceFileFromFile _ = updateCodeGeneration True

updateSourceFile _ _ = updateCodeGeneration True

handleFilesChanged :: TChan FileChange
                   -> TVar Deps
                   -> TVar IdeSession
                   -> TMVar RunCommand
                   -> TVar UpdateSessionFn
                   -> TChan LoadingStatus
                   -> IO ()
handleFilesChanged filesChanged' depsTVar sessionTVar runCommandTMVar updateSessionFnTVar loading =
  do filesChanged <- atomically (dupTChan filesChanged')
     ignoreOnceRef <- I.newIORef Set.empty
     forever $
       do fc@(FileChange filePath fileChangeType fileType) <- atomically $ readTChan filesChanged
          print fc
          ignoreOnce <- I.readIORef ignoreOnceRef
          print ignoreOnce
          let skip = FP.normalise filePath `elem` ignoreOnce
          I.modifyIORef' ignoreOnceRef (Set.delete (FP.normalise filePath))
          unless skip $
            do exists <- doesFileExist filePath
               do deps <- atomically (readTVar depsTVar)
                  let changes = runStateT (execWriterT (updatedDeps deps))
                  (depHsFiles', _) <- changes mempty
                  let depHsFiles = filter (not . Text.isInfixOf "flycheck_" . Text.pack) depHsFiles'
                  putStrLn $ "additional deps:  " ++ show depHsFiles
                  I.modifyIORef' ignoreOnceRef (Set.union (Set.fromList (map FP.normalise depHsFiles)))
                  mapM_ touchFile depHsFiles
                  case (fileChangeType, fileType) of
                    (_, CabalFile) ->
                      do putStrLn "Cabal file changed. Reloading session."
                         atomically (putTMVar runCommandTMVar Stop)
                         session <- atomically (readTVar sessionTVar)
                         shutdownSession session
                         (newSession, newUpdateSessionFn, newDeps) <- initSession loading
                         atomically $ do writeTVar sessionTVar newSession
                                         writeTVar updateSessionFnTVar newUpdateSessionFn
                                         writeTVar depsTVar newDeps
                         atomically (putTMVar runCommandTMVar Start)
                    (_, StaticFile) -> unless (null depHsFiles) updateSession
                    (_, _) -> updateSession
  where updateSession :: IO ()
        updateSession =
          do putStrLn "[Restarting Runner] Closing"
             atomically (putTMVar runCommandTMVar Stop)
             putStrLn "[Restarting Runner] Updating"
             updateSessionFn <- atomically (readTVar updateSessionFnTVar)
             void (updateSessionFn mempty)
             atomically (putTMVar runCommandTMVar Start)
             putStrLn "[Restarting Runner] Starting"
             (hsSourceDirs, _) <- checkCabalFile
             (_, newDeps) <- getDeps hsSourceDirs
             atomically (writeTVar depsTVar newDeps)

main :: IO ()
main = do
  opts@Options{..} <- getCommandLineOptions
  setEnv "PORT" "3001"
  setEnv "DISPLAY_PORT" "3000"
  loading <- newTChanIO
  loadingText <- newTVarIO ""
  filesChanged <- newTChanIO
  (session, updateSessionFn, deps) <- initSession loading
  depsTVar <- newTVarIO deps
  updateSessionFnTVar <- newTVarIO updateSessionFn
  sessionTVar <- newTVarIO session
  runnerTVar <- newTVarIO Nothing
  runCommandTMVar <- newEmptyTMVarIO
  Ex.finally
    (do _ <- async (runServer loadingText)
        _ <- async (runDevel opts sessionTVar runnerTVar runCommandTMVar)
        _ <- async (watchThread filesChanged)
        _ <- async (handleFilesChanged filesChanged depsTVar sessionTVar runCommandTMVar updateSessionFnTVar loading)
        _ <- async (handleStatusUpdates loading loadingText)
        develHsPath <- checkDevelFile
        void $ updateSessionFn (updateSourceFileFromFile develHsPath)
        putStrLn "[MAIN] Sending initial run command"
        atomically (putTMVar runCommandTMVar Start)
        putStrLn "[MAIN] Initial run command sent"
        threadDelay (1000 * 1000 * 60 * 60 * 24 * 365))  -- run devel server for up to one year
    (do runner <- atomically (readTVar runnerTVar)
        putStrLn "Shutting down the runner"
        mapM_ interrupt runner
        session' <- atomically (readTVar sessionTVar)
        shutdownSession session'
        exitSuccess)
  exitSuccess

reverseProxy :: TVar Text -> IO Application
reverseProxy loadingText = do
  mgr <- newManager defaultManagerSettings
  return $ waiProxyTo
         (const $ return $ WPRProxyDest $ ProxyDest "localhost" 3001)
         showStatus
         mgr
  where showStatus exc _ sendResponse =
          do status <- atomically $ readTVar loadingText
             sendResponse $ responseLBS
                            status502
                            [("content-type", "text/plain")]
                            (LB.fromStrict (Text.encodeUtf8 status))

runServer :: TVar Text -> IO ()
runServer loadingText = do
  appl <- reverseProxy loadingText
  run 3000 appl

initSession :: TChan LoadingStatus -> IO (IdeSession, UpdateSessionFn, Deps)
initSession loading =
  do (session, updateSessionFn) <- startSession loading
     (hsSourceDirs, _) <- checkCabalFile
     (_, deps) <- getDeps hsSourceDirs
     return (session, updateSessionFn, deps)

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
               errs <- getSourceErrors session
               when (any isError errs) (putStrLn ("[RUN DEVEL] " ++ show errs))
               unless (any isError errs)
                      (do ra <- runStmt session "Application" "develMain"
                          atomically (writeTVar runnerTVar (Just ra))
                          void (async (runLoop ra)))
               return ()
          Stop ->
            do runner <- atomically $ do runner <- readTVar runnerTVar
                                         writeTVar runnerTVar Nothing
                                         return runner
               mapM_ interrupt runner
               return ()
  where isError (SourceError{errorKind = k}) =
          case k of
            KindError -> True
            KindServerDied -> True
            KindWarning -> False



checkDevelFile :: IO String
checkDevelFile =
    loop paths
  where
    paths = ["Application.hs" {- , "app/devel.hs", "devel.hs", "src/devel.hs" -}]
    loop [] = failWith $ "file devel.hs not found, checked: " <> show paths
    loop (x:xs) = do
        e <- doesFileExist x
        if e
            then return x
            else loop xs

failWith :: String -> IO a
failWith msg = do
    putStrLn $ "ERROR: " ++ msg
    exitFailure




