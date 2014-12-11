module WhippetSpec (spec) where

import Whippet
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "removeDups" $ do
    it "returns the two lists concatenated, with duplicates removed" $ do
      removeDups ["foo", "bar", "baz"] ["buzz", "foo"] `shouldBe` ["foo", "bar", "baz", "buzz"]
