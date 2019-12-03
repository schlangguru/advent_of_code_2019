import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import ListUtil

main :: IO ()
main = hspec $ do
  describe "setAt" $ do
    it "set at begin of the list" $ do
      setAt 0 'x' "abc" `shouldBe` "xbc"

    it "set at the end of the list" $ do
      setAt 2 'x' "abc" `shouldBe` "abx"

    it "set in the middle of the list" $ do
      setAt 1 'x' "abc" `shouldBe` "axc"

    it "do nothing if index it out of bounds" $ do
      setAt 3 'x' "abc" `shouldBe` "abc"


