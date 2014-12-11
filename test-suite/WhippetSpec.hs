module WhippetSpec (spec) where

import Whippet
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "removeDups" $ do
    it "returns the two lists concatenated, with duplicates removed" $ do
      removeDups ["foo", "bar", "baz"] ["buzz", "foo"] `shouldBe` ["foo", "bar", "baz", "buzz"]

  describe "exactMatches" $ do
    it "returns exact substring matches" $ do
      exactMatches "foo" ["xfoox", "bar", "XFOOX", "foobarbaz", "baz"] `shouldBe` ["xfoox", "XFOOX", "foobarbaz"]

  describe "inexactMatches" $ do
    it "returns exact substring matches" $ do
      inexactMatches "foo" ["xfoox", "bar", "bfoazo", "fbaroo"] `shouldBe` ["xfoox", "bfoazo", "fbaroo"]
