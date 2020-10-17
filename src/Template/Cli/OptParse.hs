{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Template.Cli.OptParse
  ( -- * Interface
    getInstructions,
    Instructions (..),
    Dispatch (..),
    FillSettings (..),
    Settings (..),

    -- ** Exposed for testing
    combineToInstructions,
    getArguments,
    prefs_,
    argParser,
    parseArgs,
    parseCommand,
    parseCommandFill,
    parseFlags,
    Arguments (..),
    Command (..),
    FillArgs (..),
    Flags (..),
  )
where

import Control.Applicative
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml
import GHC.Generics (Generic)
import Options.Applicative as OptParse
import qualified Options.Applicative.Help as OptParse (string)
import Path
import Path.IO

data Instructions
  = Instructions Dispatch Settings
  deriving (Show, Eq, Generic)

getInstructions :: IO Instructions
getInstructions = do
  args@(Arguments _ flags) <- getArguments
  combineToInstructions args

-- | A product type for the settings that are common across commands
data Settings
  = Settings
      {
      }
  deriving (Show, Eq, Generic)

-- | A sum type for the commands and their specific settings
data Dispatch
  = DispatchFill FillSettings
  deriving (Show, Eq, Generic)

-- | One type per command for its settings.
-- You can omit this if the command does not need specific settings.
data FillSettings
  = FillSettings
      { fillSettingSourceDir :: !(Path Abs Dir),
        fillSettingDestinationDir :: !(Path Abs Dir),
        fillSettingFind :: !Text,
        fillSettingReplace :: !Text
      }
  deriving (Show, Eq, Generic)

-- | Combine everything to instructions
combineToInstructions :: Arguments -> IO Instructions
combineToInstructions (Arguments cmd Flags) = do
  let sets = Settings
  disp <-
    case cmd of
      CommandFill FillArgs {..} -> do
        fillSettingSourceDir <- resolveDir' $ fromMaybe "." fillArgSourceDir
        fillSettingDestinationDir <- resolveDir' $ fromMaybe "." fillArgDestinationDir
        let fillSettingFind = fillArgFind
        let fillSettingReplace = fillArgReplace
        pure $ DispatchFill FillSettings {..}
  pure $ Instructions disp sets

-- | The combination of a command with its specific flags and the flags for all commands
data Arguments
  = Arguments Command Flags
  deriving (Show, Eq, Generic)

-- | Get the command-line arguments
getArguments :: IO Arguments
getArguments = customExecParser prefs_ argParser

-- | The 'optparse-applicative' parsing preferences
prefs_ :: OptParse.ParserPrefs
prefs_ =
  -- I like these preferences. Use what you like.
  OptParse.defaultPrefs
    { OptParse.prefShowHelpOnError = True,
      OptParse.prefShowHelpOnEmpty = True
    }

-- | The @optparse-applicative@ parser for 'Arguments'
argParser :: OptParse.ParserInfo Arguments
argParser =
  OptParse.info
    (OptParse.helper <*> parseArgs)
    OptParse.fullDesc

parseArgs :: OptParse.Parser Arguments
parseArgs = Arguments <$> parseCommand <*> parseFlags

-- | A sum type for the commands and their specific arguments
data Command
  = CommandFill FillArgs
  deriving (Show, Eq, Generic)

parseCommand :: OptParse.Parser Command
parseCommand =
  OptParse.hsubparser $
    mconcat
      [ OptParse.command "fill" $ CommandFill <$> parseCommandFill
      ]

-- | One type per command, for the command-specific arguments
data FillArgs
  = FillArgs
      { fillArgSourceDir :: !(Maybe FilePath),
        fillArgDestinationDir :: !(Maybe FilePath),
        fillArgFind :: !Text,
        fillArgReplace :: !Text
      }
  deriving (Show, Eq, Generic)

-- | One 'optparse-applicative' parser for each command's flags
parseCommandFill :: OptParse.ParserInfo FillArgs
parseCommandFill = OptParse.info parser modifier
  where
    modifier = OptParse.fullDesc <> OptParse.progDesc "Fill the user"
    parser =
      ( FillArgs
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

-- | The flags that are common across commands.
data Flags
  = Flags
      {
      }
  deriving (Show, Eq, Generic)

-- | The 'optparse-applicative' parser for the 'Flags'.
parseFlags :: OptParse.Parser Flags
parseFlags = pure Flags
