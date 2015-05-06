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
import           Data.Default.Class (def)
import           Data.FileEmbed (embedFile)
import           Data.Function
import qualified Data.IORef as I
import           Data.Maybe
import           Data.Monoid
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
import           IdeSession hiding (updateSession, initSession)

import           Devel.Cabal
import           Devel.CmdLine
import           Devel.Deps

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
        isCabalFile fp = filename fp == "cabal.sandbox.config" || "cabal" `elem` extension fp
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
       do fc@(FileChange filePath fileChangeType fileType) <- atomically $ readTChan filesChanged
          print fc
          exists <- doesFileExist filePath
          lastChangedFile <- I.readIORef lastChangedFileRef
          content <- if not exists
                        then return ""
                        else LB.readFile filePath
          I.writeIORef lastChangedFileRef (Just (filePath, content))
          unless (lastChangedFile == Just (filePath, content)) $
            do deps <- atomically (readTVar depsTVar)
               let changes = runStateT (execWriterT (updatedDeps deps))
               (depHsFiles, _) <- changes mempty
               let extra = mconcat extras
                   extras = (getExtra depHsFiles)
               case (fileChangeType, fileType) of
                 (FileAdded,    HaskellFile) -> updateFile (updateSourceFileFromFile filePath <> extra)
                 (FileAdded,    DataFile)    -> updateFile (updateDataFile filePath content <> extra)
                 (FileModified, HaskellFile) -> updateFile (updateSourceFileFromFile filePath <> extra)
                 (FileModified, DataFile)    -> updateFile (updateDataFile filePath content <> extra)
                 (FileRemoved,  HaskellFile) -> updateFile (updateSourceFileDelete filePath)
                 (FileRemoved,  DataFile)    -> updateFile (updateDataFileDelete filePath)
                 (_,            CabalFile)   ->
                   do putStrLn "Cabal file changed. Reloading session."
                      atomically (putTMVar runCommandTMVar Stop)
                      session <- atomically (readTVar sessionTVar)
                      shutdownSession session
                      (newSession, newUpdateSession, newDeps) <- initSession loading
                      atomically $ do writeTVar sessionTVar newSession
                                      writeTVar updateSessionTVar newUpdateSession
                                      writeTVar depsTVar newDeps
                      develHsPath <- checkDevelFile
                      void (newUpdateSession (updateSourceFileFromFile develHsPath))
                      atomically (putTMVar runCommandTMVar Start)
  where getExtra = map (updateSourceFileDelete <> updateSourceFileFromFile)
        updateFile :: IdeSessionUpdate -> IO ()
        updateFile upd =
          do develHsPath <- checkDevelFile
             putStrLn "[Restarting Runner] Closing"
             atomically (putTMVar runCommandTMVar Stop)
             putStrLn "[Restarting Runner] Updating"
             updateSession <- atomically (readTVar updateSessionTVar)
             void (updateSession (upd <> updateSourceFileFromFile develHsPath))
             putStrLn "[Restarting Runner] Updated"
             atomically (putTMVar runCommandTMVar Start)
             putStrLn "[Restarting Runner] Starting"
             (hsSourceDirs, _) <- checkCabalFile
             (_, newDeps) <- getDeps hsSourceDirs
             atomically (writeTVar depsTVar newDeps)

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
  Ex.finally
    (do _ <- async (runDevel opts sessionTVar runnerTVar runCommandTMVar)
        _ <- async (watchThread filesChanged)
        _ <- async (handleFilesChanged filesChanged depsTVar sessionTVar runCommandTMVar updateSessionTVar loading)
        _ <- async (handleStatusUpdates loading)
        develHsPath <- checkDevelFile
        void $ updateSession (updateSourceFileFromFile develHsPath)
        atomically (putTMVar runCommandTMVar Start)
        threadDelay (1000 * 1000 * 60 * 60 * 24 * 365))  -- run devel server for up to one year
    (do runner <- atomically (readTVar runnerTVar)
        putStrLn "Shutting down the runner"
        mapM_ interrupt runner
        session' <- atomically (readTVar sessionTVar)
        shutdownSession session'
        exitSuccess)
  exitSuccess

initSession :: TChan LoadingStatus -> IO (IdeSession, UpdateSession, Deps)
initSession loading =
  do (session, updateSession) <- startSession loading
     (hsSourceDirs, _) <- checkCabalFile
     (_, deps) <- getDeps hsSourceDirs
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
               ra <- runStmt session "Application" "develMain"
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




