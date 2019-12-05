import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Wire

main :: IO ()
main = hspec $ do
  describe "Wire" $ do
    it "should build the wire" $ do
      buildWire [(1,0), (0,1), (0,1), (-1,0), (-1,0), (-1,0), (0,-1), (0,-1), (0,-1), (0,-1)]
      `shouldBe`
      [(1,0), (1,1), (1,2), (0,2), (-1,2), (-2,2), (-2,1), (-2,0), (-2,-1), (-2,-2)]
