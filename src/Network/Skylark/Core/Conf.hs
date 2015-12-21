-- |
-- Module:      Network.Skylark.Core.Conf
-- Copyright:   (c) 2015 Mark Fine
-- License:     BSD3
-- Maintainer:  Mark Fine <mark@swift-nav.com>
--
-- Conf module for Skylark Core.

module Network.Skylark.Core.Conf
  ( configFile
  , getCompleteConf
  , getDataFile
  , logLevel
  , parser
  , options
  , parseConf
  , port
  , timeout
  ) where

import Control.Monad.Logger
import Data.Aeson                   hiding (decode)
import Data.Default
import Data.Word
import Data.Yaml                    hiding (Parser, decode)
import Network.Skylark.Core.Prelude
import Network.Skylark.Core.Types
import Options.Applicative
import Paths_skylark_core
import System.Envy

--------------------------------------------------------------------------------
-- Configuration parsers

-- | Metric host
--
configFile :: Parser String
configFile =
  strOption
    $  long    "conf-file"
    <> short   'c'
    <> metavar "FILE"
    <> help    "Config file"

-- | Parse HTTP port
--
port :: Parser Word
port =
  option auto
    $  long    "port"
    <> short   'p'
    <> metavar "PORT"
    <> help    "Port to listen on"

-- | Parse connection timeout
--
timeout :: Parser Word
timeout =
  option auto
    $  long    "timeout"
    <> short   't'
    <> metavar "TIMEOUT"
    <> help    "Timeout in seconds"

-- | Parser for log level configuration.
--
logLevel :: Parser LogLevel
logLevel =
  fmap (toLogLevel . txt) $
    strOption
      $  long    "log-level"
      <> metavar "LEVEL"
      <> help    "Minimum level of logging" where
        toLogLevel "debug" = LevelDebug
        toLogLevel "info"  = LevelInfo
        toLogLevel "warn"  = LevelWarn
        toLogLevel "error" = LevelError
        toLogLevel s       = LevelOther s

-- | Parser for log level configuration.
--
parseConf :: Parser Conf
parseConf = Conf        <$>
  optional configFile   <*>
  optional port         <*>
  optional timeout      <*>
  optional logLevel

-- | Produce a full command line options parser.
--
parser :: Parser a -> ParserInfo a
parser parse = info ( helper <*> parse ) fullDesc

-- | Execute parser and value from the parser.
--
options :: ParserInfo a -> IO a
options = execParser

-- | Execute parser and value from the parser.
--
getDataFile :: FromJSON a => String -> IO a
getDataFile f =
  getDataFileName f >>= decodeFileEither >>= either throwIO return

-- | Return the full environmental configuration. Looks for the
-- configuration in three places (in order): a default, a
-- configuration file, command line options, and the environmental
-- configuration. Accepts the last non-Maybe value in each
--
getCompleteConf :: (Monoid a, FromEnv a, Default a, FromJSON a) => ParserInfo a -> (a -> Maybe String) -> IO a
getCompleteConf p conf = do
  e <- decode
  o <- options p
  let d = def
  f <- maybe (return Nothing) getDataFile $ conf $ d <> o <> fromMaybe mempty e
  return $ d <> fromMaybe mempty f <> o <> fromMaybe mempty e
