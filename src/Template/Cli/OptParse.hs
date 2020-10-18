{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Template.Cli.OptParse
  ( -- * Interface
    getSettings,
    Settings (..),
    FromTo (..),

    -- ** Exposed for testing
    combineToSettings,
    parseFlags,
    Flags (..),
  )
where

import Control.Applicative
import Data.Maybe
import Data.Text (Text)
import Data.Time
import GHC.Generics (Generic)
import Options.Applicative as OptParse
import Path
import Path.IO
import System.Directory as FP
import System.Exit

getSettings :: IO Settings
getSettings = getFlags >>= combineToSettings

data Settings
  = Settings
      { settingFromTo :: !FromTo,
        settingBackupDir :: !(Path Abs Dir),
        settingFind :: !Text,
        settingReplace :: !Text,
        settingOverwrite :: Bool
      }
  deriving (Show, Eq, Generic)

data FromTo
  = FromToDir (Path Abs Dir) (Path Abs Dir)
  | FromToFile (Path Abs File) (Path Abs File)
  deriving (Show, Eq, Generic)

combineToSettings :: Flags -> IO Settings
combineToSettings Flags {..} = do
  settingBackupDir <- case flagBackupDir of
    Nothing -> do
      dataDir <- getXdgDir XdgData (Just [reldir|template|])
      now <- getCurrentTime
      resolveDir dataDir $ formatTime defaultTimeLocale "%F %T" now
    Just bd -> resolveDir' bd
  let sourceFP = fromMaybe "." flagSource
  let destinationFP = fromMaybe "." flagDestination
  source <- resolveFileOrDir sourceFP
  destination <- resolveFileOrDir destinationFP
  settingFromTo <- case (source, destination) of
    (NonExistent sfp, _) -> die $ "The source does not exist: " <> sfp
    (IsFile sf, IsFile df) -> pure $ FromToFile sf df
    (IsDir sd, IsDir dd) -> pure $ FromToDir sd dd
    (IsFile sf, IsDir dd) -> pure $ FromToFile sf (dd </> filename sf)
    (IsDir _, IsFile df) -> die $ "The source is a directory but the destination is a file: " <> fromAbsFile df
    (IsFile sf, NonExistent dfp) -> FromToFile sf <$> resolveFile' dfp
    (IsDir sd, NonExistent dfp) -> FromToDir sd <$> resolveDir' dfp
  let settingFind = flagFind
  let settingReplace = flagReplace
  let settingOverwrite = flagOverwrite
  pure Settings {..}

data FileOrDir = IsFile (Path Abs File) | IsDir (Path Abs Dir) | NonExistent FilePath

resolveFileOrDir :: FilePath -> IO FileOrDir
resolveFileOrDir fp = do
  dirExists <- FP.doesDirectoryExist fp
  if dirExists
    then IsDir <$> resolveDir' fp
    else do
      fileExists <- FP.doesFileExist fp
      if fileExists
        then IsFile <$> resolveFile' fp
        else pure $ NonExistent fp

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
      { flagSource :: !(Maybe FilePath),
        flagDestination :: !(Maybe FilePath),
        flagBackupDir :: !(Maybe FilePath),
        flagFind :: !Text,
        flagReplace :: !Text,
        flagOverwrite :: !Bool
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
                    [ long "source",
                      help "The file or directory to read from",
                      metavar "DIRECTORY"
                    ]
                )
            )
          <*> optional
            ( strOption
                ( mconcat
                    [ long "destination",
                      help "The file or directory to write to",
                      metavar "DIRECTORY"
                    ]
                )
            )
          <*> optional
            ( strOption
                ( mconcat
                    [ long "backup-dir",
                      help "The directory to write a backup to",
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
          <*> switch
            ( mconcat
                [ long "overwrite",
                  help "The overwrite files when writing, default: false"
                ]
            )
      )
