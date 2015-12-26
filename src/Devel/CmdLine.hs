{-# LANGUAGE OverloadedStrings #-}
module Devel.CmdLine (
    -- * Types
    Options(..)
    -- * Parsing
  , getCommandLineOptions
  ) where

import Data.Monoid
import Options.Applicative

import IdeSession

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

data Options =
  Options {eventTimeout :: Int -- negative value for no timeout
          ,successHook :: Maybe String
          ,failHook :: Maybe String
          ,buildDir :: Maybe String
          ,develPort :: Int
          ,develTlsPort :: Int
          ,proxyTimeout :: Int}
  deriving (Show,Eq)

data DevelTermOpt = TerminateOnEnter | TerminateOnlyInterrupt
     deriving (Show, Eq)

{-------------------------------------------------------------------------------
  Parsers
-------------------------------------------------------------------------------}

parseOptions :: Parser Options
parseOptions = Options <$> option auto ( long "event-timeout" <> short 't' <> value defaultRescan <> metavar "N"
                              <> help ("Force rescan of files every N seconds (default "
                                       ++ show defaultRescan
                                       ++ ", use -1 to rely on FSNotify alone)") )
                       <*> optStr ( long "success-hook" <> short 's' <> metavar "COMMAND"
                              <> help "Run COMMAND after rebuild succeeds")
                       <*> optStr ( long "failure-hook" <> short 'f' <> metavar "COMMAND"
                              <> help "Run COMMAND when rebuild fails")
                       <*> optStr ( long "builddir" <> short 'b'
                              <> help "Set custom cabal build directory, default `dist'")
                       <*> option auto ( long "port" <> short 'p' <> value 3000 <> metavar "N"
                              <> help "Devel server listening port" )
                       <*> option auto ( long "tls-port" <> short 'q' <> value 3443 <> metavar "N"
                              <> help "Devel server listening port (tls)" )
                       <*> option auto ( long "proxy-timeout" <> short 'x' <> value 0 <> metavar "N"
                              <> help "Devel server timeout before returning 'not ready' message (in seconds, 0 for none)" )

defaultRescan :: Int
defaultRescan = 10

optStr :: Mod OptionFields (Maybe String) -> Parser (Maybe String)
optStr m = option (Just <$> str) $ value Nothing <> m

{-------------------------------------------------------------------------------
  Top-level API
-------------------------------------------------------------------------------}

getCommandLineOptions :: IO Options
getCommandLineOptions = execParser opts
  where
    opts = info
      (helper <*> parseOptions)
      (mconcat [
          fullDesc
        , progDesc "Start a new session"
        , header "ide-backend-client: JSON interface to ide-backend"
        ])

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- deriving instance Show SessionConfig

instance Show (a -> b) where
  show _ = "<<function>>"
