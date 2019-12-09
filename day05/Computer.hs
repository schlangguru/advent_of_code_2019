module Computer (
  createComputer,
  process,
  getMemory,
  getOutput,
) where

import Data.Sequence (Seq(..), (<|), (|>), (><))
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Foldable

data Computer = Computer {
  state :: State,
  memory :: Memory,
  instPointer :: InstPointer,
  input :: Input,
  output :: Output
} deriving (Show)

data State = Running | Stopped deriving (Show, Eq)
type Memory = Seq Int
type InstPointer = Int
type Input = [Int]
type Output = [Int]

data Operation = ADD | MUL | INPUT | OUTPUT | HALT deriving (Show, Eq)
type Argument = Int
data Instruction = Instruction {
  operation :: Operation,
  arguments :: [Argument]
}

createComputer :: [Int] -> Input -> Computer
createComputer memory input = Computer {
  state = Running,
  memory = Seq.fromList memory,
  instPointer = 0,
  input = input,
  output = []
}

getMemory :: Computer -> [Int]
getMemory computer = Foldable.toList $ memory computer

getOutput :: Computer -> [Int]
getOutput computer = output computer

process :: Computer -> Computer
process computer@Computer{state = state}
  | state == Running = process $ processInstruction computer
  | state == Stopped = computer

processInstruction :: Computer -> Computer
processInstruction computer@Computer{state = state, memory = memory, instPointer = instPointer, input = input, output = output} =
  let
    instruction@(Instruction operation args) = decodeInstruction memory instPointer
    newInstPointer = instPointer + 1 + (length $ arguments instruction)
    newState = decodeState operation

    newMemory = case operation of
                  ADD    -> apply (+) memory args
                  MUL    -> apply (*) memory args
                  INPUT  -> writeMemory (args!!0) (head input) memory
                  _      -> memory

    newInput = case operation of
                  INPUT -> tail input
                  _     -> input

    newOutput = case operation of
                  OUTPUT  -> output ++ [readMemory memory (args!!0)]
                  _       -> output
  in
    Computer {
      state = newState,
      memory = newMemory,
      instPointer = newInstPointer,
      input = newInput,
      output = newOutput
    }

decodeState :: Operation -> State
decodeState HALT = Stopped
decodeState _    = Running

decodeInstruction :: Memory -> InstPointer -> Instruction
decodeInstruction memory instPointer =
  let
    opCode = readMemory memory instPointer
    (operation, nmbOfArgs) = decodeOpcode opCode
    args = case nmbOfArgs of
              0 -> []
              _ -> map (\i -> readMemory memory i) [instPointer + 1 .. instPointer + nmbOfArgs]
  in
    Instruction {
      operation = operation,
      arguments = args
    }

readMemory :: Memory -> InstPointer -> Int
readMemory memory instPointer = Seq.index memory instPointer

writeMemory :: InstPointer -> Int -> Memory -> Memory
writeMemory instPointer elem memory = Seq.update instPointer elem memory

decodeOpcode :: Int -> (Operation, Int)
decodeOpcode 1  = (ADD, 3)
decodeOpcode 2  = (MUL, 3)
decodeOpcode 3  = (INPUT, 1)
decodeOpcode 4  = (OUTPUT, 1)
decodeOpcode 99 = (HALT, 0)
decodeOpcode _  = (HALT, 0)


apply :: (Int -> Int -> Int) -> Memory -> [Argument] -> Memory
apply f memory args =
  let
    op0 = Seq.index memory (args!!0)
    op1 = Seq.index memory (args!!1)
    result = f op0 op1
  in
    Seq.update (args!!2) result memory