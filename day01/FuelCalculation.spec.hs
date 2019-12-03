import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import FuelCalculation

main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "calculates the fuel for a mass of 0" $ do
      calcFuel 0  `shouldBe` 0

    it "calculates the fuel for a mass of 8" $
      calcFuel 8  `shouldBe` 0

    it "calculates the fuel for a mass of 12" $ do
      calcFuel 12  `shouldBe` 2

    it "calculates the fuel for a mass of 14" $ do
      calcFuel 12  `shouldBe` 2

    it "calculates the fuel for a mass of 1969" $ do
      calcFuel 1969  `shouldBe` 966

    it "calculates the fuel for a mass of 100756" $ do
      calcFuel 100756  `shouldBe` 50346

