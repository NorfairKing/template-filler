{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Template.CliSpec
  ( spec,
  )
where

import qualified Data.ByteString as SB
import qualified Data.DirForest as DF
import Data.List
import Path
import Path.IO
import Template.Cli
import Template.Cli.OptParse
import Test.Hspec
import Text.Casing

spec :: Spec
spec = do
  describe "casings"
    $ it "contains distinct casings"
    $ do
      let input = Identifier ["foo", "bar"]
          results = map ($ input) casings
      mapM_ print results
      sort (nub results) `shouldBe` sort results
  describe "fillTextViaCasing" $ do
    it "replaces PascalCase" $
      fillTextViaCasing "FooBar" "BazQuux" toPascal "FooBar" `shouldBe` "BazQuux"
    it "replaces CamelCase" $
      fillTextViaCasing "FooBar" "BazQuux" toCamel "fooBar" `shouldBe` "bazQuux"
    it "replaces kebab-case" $
      fillTextViaCasing "FooBar" "BazQuux" toKebab "foo-bar" `shouldBe` "baz-quux"
    it "replaces quiet_snake_case" $
      fillTextViaCasing "FooBar" "BazQuux" toQuietSnake "foo_bar" `shouldBe` "baz_quux"
    it "replaces STREAMING_SNAKE_CASE" $
      fillTextViaCasing "FooBar" "BazQuux" toScreamingSnake "FOO_BAR" `shouldBe` "BAZ_QUUX"
    it "replaces word case" $
      fillTextViaCasing "FooBar" "BazQuux" toWordsLower "foo bar" `shouldBe` "baz quux"
    it "replaces word case" $
      fillTextViaCasing "FooBar" "BazQuux" toWordsCapitalised "Foo Bar" `shouldBe` "Baz Quux"
    it "replaces slash case" $
      fillTextViaCasing "FooBar" "BazQuux" toSlashedLower "foo/bar" `shouldBe` "baz/quux"
    it "replaces slash case" $
      fillTextViaCasing "FooBar" "BazQuux" toSlashedCapitalised "Foo/Bar" `shouldBe` "Baz/Quux"
    it "replaces dot case" $
      fillTextViaCasing "FooBar" "BazQuux" toDottedLower "foo.bar" `shouldBe` "baz.quux"
    it "replaces dot case" $
      fillTextViaCasing "FooBar" "BazQuux" toDottedCapitalised "Foo.Bar" `shouldBe` "Baz.Quux"
  describe "fillText" $ do
    it "replaces this example" $
      fillText "foo" "bar" "hello foo world" `shouldBe` "hello bar world"
    it "replaces PascalCase" $
      fillText "FooBar" "BazQuux" "FooBar" `shouldBe` "BazQuux"
    it "replaces CamelCase" $
      fillText "FooBar" "BazQuux" "fooBar" `shouldBe` "bazQuux"
    it "replaces kebab-case" $
      fillText "FooBar" "BazQuux" "foo-bar" `shouldBe` "baz-quux"
    it "replaces quiet_snake_case" $
      fillText "FooBar" "BazQuux" "foo_bar" `shouldBe` "baz_quux"
    it "replaces STREAMING_SNAKE_CASE" $
      fillText "FooBar" "BazQuux" "FOO_BAR" `shouldBe` "BAZ_QUUX"
    it "replaces word case" $
      fillText "FooBar" "BazQuux" "foo bar" `shouldBe` "baz quux"
    it "replaces word case" $
      fillText "FooBar" "BazQuux" "Foo Bar" `shouldBe` "Baz Quux"
    it "replaces lower slash case" $
      fillText "FooBar" "BazQuux" "foo/bar" `shouldBe` "baz/quux"
    it "replaces capitalised slash case" $
      fillText "FooBar" "BazQuux" "Foo/Bar" `shouldBe` "Baz/Quux"
    it "replaces lower dot case" $
      fillText "FooBar" "BazQuux" "foo.bar" `shouldBe` "baz.quux"
    it "replaces capitalised dot case" $
      fillText "FooBar" "BazQuux" "Foo.Bar" `shouldBe` "Baz.Quux"
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
                { settingFromTo = FromToDir sourceDir destinationDir,
                  settingBackupDir = backupDir,
                  settingFind = "Foo",
                  settingReplace = "Bar",
                  settingOverwrite = True
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
                { settingFromTo = FromToDir sourceDir destinationDir,
                  settingBackupDir = backupDir,
                  settingFind = "Foo",
                  settingReplace = "Bar",
                  settingOverwrite = True
                }
            actual <- DF.read destinationDir (\p -> SB.readFile (fromAbsFile p))
            actual `shouldBe` expected
