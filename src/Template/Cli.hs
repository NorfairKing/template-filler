{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Template.Cli where

import qualified Data.DirForest as DF
import Data.DirForest (DirForest (..))
import qualified Data.Map as M
import Data.Map (Map)
import Data.Monoid
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Time
import Path
import Path.IO
import System.Exit
import Template.Cli.OptParse
import Text.Casing
import Text.Show.Pretty

templateCli :: IO ()
templateCli = getSettings >>= fill

fill :: Settings -> IO ()
fill fs@Settings {..} = do
  df <- DF.readNonHidden settingSourceDir (\p -> T.readFile $ fromAbsFile p)
  case DF.fromMap $ fillMap settingFind settingReplace $ DF.toMap df of
    Left err -> die $ "Failed to replace path: " <> show err
    Right pathsReplacedDF -> do
      let df' = fillDirforest settingFind settingReplace pathsReplacedDF
      DF.write settingBackupDir df (\p t -> T.writeFile (fromAbsFile p) t)
      DF.write settingDestinationDir df' (\p t -> T.writeFile (fromAbsFile p) t)

fillMap :: Text -> Text -> Map FilePath a -> Map FilePath a
fillMap findText replaceText = M.mapKeys (T.unpack . fillText findText replaceText . T.pack)

fillDirforest :: Text -> Text -> DirForest Text -> DirForest Text
fillDirforest findText replaceText df = fillText findText replaceText <$> df

fillText :: Text -> Text -> Text -> Text
fillText findText replaceText = appEndo $ mconcat $ map (Endo . replaceUsingCasing) casings
  where
    casings :: [Identifier String -> String]
    casings = [toCamel, toPascal, toSnake, toQuietSnake, toScreamingSnake, toKebab]
    viaCasing :: (Identifier String -> String) -> Text -> Text
    viaCasing casing = T.pack . casing . fromAny . T.unpack
    replaceUsingCasing :: (Identifier String -> String) -> Text -> Text
    replaceUsingCasing casing =
      let needle = viaCasing casing findText
          replacement = viaCasing casing replaceText
       in T.replace needle replacement
