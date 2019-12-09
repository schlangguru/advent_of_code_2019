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
data ParameterMode = Position | Immediate deriving (Show, Eq)
type Argument = (ParameterMode, Int)
data Instruction = Instruction {
  operation :: Operation,
  arguments :: [Argument]
} deriving (Show)

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
                  ADD    -> doMath (+) memory args
                  MUL    -> doMath (*) memory args
                  INPUT  -> doInput (args!!0) (head input) memory
                  _      -> memory

    newInput = case operation of
                  INPUT -> tail input
                  _     -> input

    newOutput = case operation of
                  OUTPUT  -> output ++ [doOutput (args!!0) memory]
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
    opCode = readMemory instPointer memory
    (operation, nmbOfArgs) = decodeOpcode $ opCode `mod` 100
    paramModes = parseParameterModes opCode nmbOfArgs
    args = case nmbOfArgs of
              0 -> []
              _ -> map (\i -> (paramModes!!(i-1), readMemory (instPointer + i) memory)) [1 .. nmbOfArgs]
  in
    Instruction {
      operation = operation,
      arguments = args
    }

readMemory :: InstPointer -> Memory -> Int
readMemory instPointer memory = Seq.index memory instPointer

writeMemory :: InstPointer -> Int -> Memory -> Memory
writeMemory instPointer elem memory = Seq.update instPointer elem memory

decodeOpcode :: Int -> (Operation, Int)
decodeOpcode 1  = (ADD, 3)
decodeOpcode 2  = (MUL, 3)
decodeOpcode 3  = (INPUT, 1)
decodeOpcode 4  = (OUTPUT, 1)
decodeOpcode 99 = (HALT, 0)
decodeOpcode _  = (HALT, 0)

doMath :: (Int -> Int -> Int) -> Memory -> [Argument] -> Memory
doMath f memory args =
  let
    (op0:op1:_) = map (\arg -> readArgumentValue arg memory) args
    result = f op0 op1
    (_, pos) = args!!2
  in
    writeMemory pos result memory

doInput :: Argument -> Int -> Memory -> Memory
doInput (_, pos) val memory = writeMemory pos val memory

doOutput :: Argument -> Memory -> Int
doOutput (_, pos) memory = readMemory pos memory

parseParameterModes :: Int -> Int -> [ParameterMode]
parseParameterModes code nmb =
  let
    modes = map toParameterMode $ reverse $ show $ code `div` 100
  in modes ++ replicate (nmb - length modes) Position

toParameterMode :: Char -> ParameterMode
toParameterMode '0' = Position
toParameterMode '1' = Immediate

readArgumentValue :: Argument -> Memory -> Int
readArgumentValue (paramMode, arg) memory
  | paramMode == Position = readMemory arg memory
  | otherwise             = arg