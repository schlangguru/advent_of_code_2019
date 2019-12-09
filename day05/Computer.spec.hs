import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Computer

main :: IO ()
main = hspec $ do
  describe "Computer" $ do
    it "processes a simple ADD" $ do
      let intCodes = [1,0,0,0,99]
      let expected = [2,0,0,0,99]
      let result = getMemory $ process $ createComputer intCodes []
      result `shouldBe` expected

    it "processes a simple MUL" $ do
      let intCodes = [2,3,0,3,99]
      let expected = [2,3,0,6,99]
      let result = getMemory $ process $ createComputer intCodes []
      result `shouldBe` expected

    it "processes multiple Opcodes" $ do
      let intCodes = [1,1,1,4,99,5,6,0,99]
      let expected = [30,1,1,4,2,5,6,0,99]
      let result = getMemory $ process $ createComputer intCodes []
      result `shouldBe` expected

    it "processes an input" $ do
      let intCodes = [3,0,99]
      let input = [42]
      let expected = [42,0,99]
      let result = getMemory $ process $ createComputer intCodes input
      result `shouldBe` expected

    it "processes multile inputs" $ do
      let intCodes = [3,0,3,3,99]
      let input = [42,84]
      let expected = [42,0,3,84,99]
      let result = getMemory $ process $ createComputer intCodes input
      result `shouldBe` expected

    it "processes an output" $ do
      let intCodes = [4,3,99,42]
      let expectedMem = [4,3,99,42]
      let expectedOut = [42]
      let computer = process $ createComputer intCodes []
      getMemory computer `shouldBe` expectedMem
      getOutput computer `shouldBe` expectedOut

    it "processes multiple output" $ do
      let intCodes = [4,0,4,4,99]
      let expectedMem = [4,0,4,4,99]
      let expectedOut = [4,99]
      let computer = process $ createComputer intCodes []
      getMemory computer `shouldBe` expectedMem
      getOutput computer `shouldBe` expectedOut