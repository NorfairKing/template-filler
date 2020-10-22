{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Template.Cli where

import Control.Monad
import qualified Data.ByteString as SB
import Data.ByteString (ByteString)
import qualified Data.DirForest as DF
import Data.DirForest (DirForest (..))
import Data.List
import qualified Data.Map as M
import Data.Map (Map)
import Data.Monoid
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as T
import Data.Time
import Path
import Path.IO
import System.Exit
import System.IO
import System.IO.Error
import Template.Cli.OptParse
import Text.Casing
import Text.Show.Pretty

templateCli :: IO ()
templateCli = getSettings >>= fill

fill :: Settings -> IO ()
fill fs@Settings {..} =
  case settingFromTo of
    FromToFile source destination -> fillFile source destination
    FromToDir source destination -> fillDir source destination
  where
    writeSafe :: Path Abs File -> ByteString -> IO ()
    writeSafe p bs = do
      fileExists <- doesFileExist p
      if fileExists && not settingOverwrite
        then pure ()
        else
          SB.writeFile (fromAbsFile p) bs
            `catchIOError` ( \e ->
                               if isPermissionError e
                                 then pure ()
                                 else ioError e
                           )
    fillFile :: Path Abs File -> Path Abs File -> IO ()
    fillFile source destination = do
      mbs <- forgivingAbsence $ SB.readFile $ fromAbsFile source
      let mbs' = fillByteString settingFind settingReplace <$> mbs
      forM_ mbs' $ writeSafe destination
    fillDir :: Path Abs Dir -> Path Abs Dir -> IO ()
    fillDir source destination = do
      df <- DF.readFiltered (const True) (\d -> not $ ".git" `isInfixOf` fromAbsDir d) source (\p -> SB.readFile $ fromAbsFile p)
      case DF.fromMap $ fillMap settingFind settingReplace $ DF.toMap df of
        Left err -> die $ "Failed to replace path: " <> show err
        Right pathsReplacedDF -> do
          let df' = fillDirforest settingFind settingReplace pathsReplacedDF
          DF.write (settingBackupDir </> [reldir|source|]) df (\p t -> SB.writeFile (fromAbsFile p) t)
          destExists <- doesDirExist destination
          when destExists $ copyDirRecur destination (settingBackupDir </> [reldir|destination|])
          DF.write destination df' writeSafe

fillMap :: Text -> Text -> Map FilePath a -> Map FilePath a
fillMap findText replaceText = M.mapKeys (T.unpack . fillText findText replaceText . T.pack)

fillDirforest :: Text -> Text -> DirForest ByteString -> DirForest ByteString
fillDirforest findText replaceText df = fillByteString findText replaceText <$> df

fillByteString :: Text -> Text -> ByteString -> ByteString
fillByteString findText replaceText bs = case TE.decodeUtf8' bs of
  Left _ -> bs
  Right t -> TE.encodeUtf8 $ fillText findText replaceText t

fillText :: Text -> Text -> Text -> Text
fillText findText replaceText = appEndo $ mconcat $ map (Endo . replaceUsingCasing) casings
  where
    casings :: [Identifier String -> String]
    casings = [toCamel, toPascal, toSnake, toQuietSnake, toScreamingSnake, toKebab]
    viaCasing :: (Identifier String -> String) -> Text -> Text
    viaCasing casing = T.pack . casing . fromHumps . T.unpack
    replaceUsingCasing :: (Identifier String -> String) -> Text -> Text
    replaceUsingCasing casing =
      let needle = viaCasing casing findText
          replacement = viaCasing casing replaceText
       in T.replace needle replacement
