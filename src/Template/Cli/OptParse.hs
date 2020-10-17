{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Template.Cli.OptParse
  ( -- * Interface
    getSettings,
    Settings (..),

    -- ** Exposed for testing
    combineToSettings,
    parseFlags,
    Flags (..),
  )
where

import Control.Applicative
import Data.Maybe
import Data.Text (Text)
import GHC.Generics (Generic)
import Options.Applicative as OptParse
import Path
import Path.IO

getSettings :: IO Settings
getSettings = getFlags >>= combineToSettings

data Settings
  = Settings
      { settingSourceDir :: !(Path Abs Dir),
        settingDestinationDir :: !(Path Abs Dir),
        settingFind :: !Text,
        settingReplace :: !Text
      }
  deriving (Show, Eq, Generic)

combineToSettings :: Flags -> IO Settings
combineToSettings Flags {..} = do
  settingSourceDir <- resolveDir' $ fromMaybe "." flagSourceDir
  settingDestinationDir <- resolveDir' $ fromMaybe "." flagDestinationDir
  let settingFind = flagFind
  let settingReplace = flagReplace
  pure Settings {..}

getFlags :: IO Flags
getFlags = customExecParser prefs_ parseFlags

prefs_ :: OptParse.ParserPrefs
prefs_ =
  OptParse.defaultPrefs
    { OptParse.prefShowHelpOnError = True,
      OptParse.prefShowHelpOnEmpty = True
    }

data Flags
  = Flags
      { flagSourceDir :: !(Maybe FilePath),
        flagDestinationDir :: !(Maybe FilePath),
        flagFind :: !Text,
        flagReplace :: !Text
      }
  deriving (Show, Eq, Generic)

parseFlags :: OptParse.ParserInfo Flags
parseFlags = OptParse.info parser modifier
  where
    modifier = OptParse.fullDesc <> OptParse.progDesc "Fill a template"
    parser =
      ( Flags
          <$> optional
            ( strOption
                ( mconcat
                    [ long "source-dir",
                      help "The directory to read from",
                      metavar "DIRECTORY"
                    ]
                )
            )
          <*> optional
            ( strOption
                ( mconcat
                    [ long "destination-dir",
                      help "The directory to write to",
                      metavar "DIRECTORY"
                    ]
                )
            )
          <*> strOption
            ( mconcat
                [ long "find",
                  help "The string to find",
                  metavar "PATTERN"
                ]
            )
          <*> strOption
            ( mconcat
                [ long "replace",
                  help "The string to replace by",
                  metavar "REPLACEMENT"
                ]
            )
      )
