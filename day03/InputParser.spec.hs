import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import InputParser

main :: IO ()
main = hspec $ do
  describe "InputParser" $ do
    it "should parse the input" $ do
      parseInput "R1,U2,L3,D4" `shouldBe` [(1,0), (0,1), (0,1), (-1,0), (-1,0), (-1,0), (0,-1), (0,-1), (0,-1), (0,-1)]




