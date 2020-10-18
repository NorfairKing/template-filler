{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Template.CliSpec
  ( spec,
  )
where

import qualified Data.ByteString as SB
import qualified Data.DirForest as DF
import Path
import Path.IO
import Template.Cli
import Template.Cli.OptParse
import Test.Hspec

spec :: Spec
spec = do
  describe "fillText" $ do
    it "replaces this example" $
      fillText "foo" "bar" "hello foo world" `shouldBe` "hello bar world"
  describe "fill" $ do
    it "works for this simple example"
      $ withSystemTempDir "template"
      $ \sourceDir ->
        withSystemTempDir "template" $ \destinationDir ->
          withSystemTempDir "template" $ \backupDir -> do
            let input = DF.singletonFile [relfile|"foofile.txt"|] "foobar"
            let expected = DF.singletonFile [relfile|"barfile.txt"|] "barbar"
            DF.write sourceDir input (\p sb -> SB.writeFile (fromAbsFile p) sb)
            fill
              Settings
                { settingSourceDir = sourceDir,
                  settingDestinationDir = destinationDir,
                  settingBackupDir = backupDir,
                  settingFind = "Foo",
                  settingReplace = "Bar"
                }
            actual <- DF.read destinationDir (\p -> SB.readFile (fromAbsFile p))
            actual `shouldBe` expected
    it "works for this example with binary data"
      $ withSystemTempDir "template"
      $ \sourceDir ->
        withSystemTempDir "template" $ \destinationDir ->
          withSystemTempDir "template" $ \backupDir -> do
            let binaryData = SB.pack [0xc3, 0x28]
            let input = DF.singletonFile [relfile|"foofile.txt"|] binaryData
            let expected = DF.singletonFile [relfile|"barfile.txt"|] binaryData
            DF.write sourceDir input (\p sb -> SB.writeFile (fromAbsFile p) sb)
            fill
              Settings
                { settingSourceDir = sourceDir,
                  settingDestinationDir = destinationDir,
                  settingBackupDir = backupDir,
                  settingFind = "Foo",
                  settingReplace = "Bar"
                }
            actual <- DF.read destinationDir (\p -> SB.readFile (fromAbsFile p))
            actual `shouldBe` expected
