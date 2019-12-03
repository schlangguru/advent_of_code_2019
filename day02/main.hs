import Computer
import StringUtil
import ListUtil

import Data.List
import Data.Maybe

partOne :: IO()
partOne = do
  input <- readFile "input.txt"
  let cmds = parseCmds (12, 2) input
  print $ process (cmds, cmds)

partTwo :: IO()
partTwo = do
    input <- readFile "input.txt"
    let intCodes = [let cmds = parseCmds (i, j) input in process (cmds, cmds) | i <- [0..99], j <- [0..99]]
    let intCode = fromJust $ find (\ic -> head ic == 19690720) intCodes
    print intCode

parseCmds :: (Int, Int) -> String -> [Int]
parseCmds (noun, verb) input =
  setAt 1 noun $
  setAt 2 verb $
  map (read::String->Int) $ split ',' input

main :: IO()
main = partTwo
