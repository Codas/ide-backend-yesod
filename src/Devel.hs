module Main where

import Control.Concurrent.STM
import Control.Concurrent (forkIO)
import Control.Exception
import Control.Monad (join, mfilter)
import Control.Arrow ((***))
import Data.Function
import Data.List (sortBy)
import Data.Monoid
import Data.Ord
import Data.Text (Text)
import Prelude hiding (mod, span)
import System.IO
import System.Directory
import System.Exit ( ExitCode (..)
                   , exitFailure
                   , exitSuccess)

import qualified Data.Text as Text

import IdeSession
import Devel.Cabal
import Devel.CmdLine

main :: IO ()
main = do
  opts@Options{..} <- getCommandLineOptions
  loading <- newTVarIO NotLoading
  session <- startCabalSession opts loading
  return ()

startCabalSession :: Options -> TVar LoadingStatus -> IO ()
startCabalSession opts loading =
  bracket (startSession loading)
          shutdownSession
          (mainLoop opts )

mainLoop :: Options -> IdeSession -> IO ()
mainLoop opts session =
  do develHsPath <- checkDevelFile
     updateSession session
                   (updateSourceFileFromFile develHsPath)
                   print
     getSourceErrors session >>= print

     putStrLn "Calling runStmt"
     ra <- runStmt session "Main" "main"
     putStrLn "Finished runStmt"
     let loop = do putStrLn "Calling runWait"
                   x <- runWait ra
                   putStrLn $ "Fnished runWait: " ++ show x
                   case x of
                       Left _ -> loop
                       Right _ -> return ()
     _loopThread <- forkIO loop
     
     return ()

checkDevelFile :: IO FilePath
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
