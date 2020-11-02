{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Template.Cli where

import Control.Monad
import qualified Data.ByteString as SB
import Data.ByteString (ByteString)
import Data.Char as Char
import qualified Data.DirForest as DF
import Data.DirForest (DirForest (..))
import Data.List
import qualified Data.Map as M
import Data.Map (Map)
import Data.Monoid
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Path
import Path.IO
import System.Exit
import System.IO.Error
import Template.Cli.OptParse
import Text.Casing

templateCli :: IO ()
templateCli = getSettings >>= fill

fill :: Settings -> IO ()
fill Settings {..} =
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
fillText findText replaceText = appEndo $ mconcat $ map (Endo . fillTextViaCasing findText replaceText) casings

fillTextViaCasing :: Text -> Text -> (Identifier String -> String) -> Text -> Text
fillTextViaCasing findText replaceText casing =
  let needle = viaCasing casing findText
      replacement = viaCasing casing replaceText
   in T.replace needle replacement

normaliseIdentifier :: Identifier String -> Identifier String
normaliseIdentifier = Identifier . map (map Char.toLower) . unIdentifier

viaCasing :: (Identifier String -> String) -> Text -> Text
viaCasing casing = T.pack . casing . normaliseIdentifier . fromHumps . T.unpack

-- The order matters
casings :: [Identifier String -> String]
casings =
  [ toCamel,
    toDottedCapitalised,
    toDottedLower,
    toKebab,
    toPascal,
    toQuietSnake,
    toScreamingSnake,
    toSlashedCapitalised,
    toSlashedLower,
    toWordsCapitalised,
    toWordsLower
  ]

toWordsLower :: Identifier String -> String
toWordsLower = toWords

toWordsCapitalised :: Identifier String -> String
toWordsCapitalised = unwords . map wordCase . unIdentifier

toDottedLower :: Identifier String -> String
toDottedLower = intercalate "." . unIdentifier

toDottedCapitalised :: Identifier String -> String
toDottedCapitalised = intercalate "." . map wordCase . unIdentifier

toSlashedLower :: Identifier String -> String
toSlashedLower = intercalate "/" . unIdentifier

toSlashedCapitalised :: Identifier String -> String
toSlashedCapitalised = intercalate "/" . map wordCase . unIdentifier

wordCase :: String -> String
wordCase "" = ""
wordCase (x : xs) = toUpper x : map toLower xs
