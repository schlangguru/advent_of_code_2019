import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Rules

main :: IO ()
main = hspec $ do
  describe "Rules" $ do
    it "checks 6 digits in a number" $ do
      hasSixDigits 123456 `shouldBe` True
      hasSixDigits 0      `shouldBe` False
      hasSixDigits 1234567 `shouldBe` False

    it "checks that no digit decreases" $ do
      digitsDontDecrease 123456 `shouldBe` True
      digitsDontDecrease 111111 `shouldBe` True
      digitsDontDecrease 111999 `shouldBe` True
      digitsDontDecrease 123546 `shouldBe` False
      digitsDontDecrease 654321 `shouldBe` False

    it "checks double (or more) adjacent digits" $ do
      hasMultiDigit 123456 `shouldBe` False
      hasMultiDigit 124456 `shouldBe` True
      hasMultiDigit 111111 `shouldBe` True

    it "checks double (exact) adjacent digits" $ do
      hasDoubleDigit 112233 `shouldBe` True
      hasDoubleDigit 123444 `shouldBe` False
      hasDoubleDigit 111122 `shouldBe` True

