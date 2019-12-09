import Data.List.Split
import Computer

partOne :: IO()
partOne = do
  puzzleInput <- readFile "input.txt"
  let intCodes = map (read::String->Int) $ splitOn "," puzzleInput
  let computer = createComputer intCodes [1]
  print $ last $ getOutput $ process computer

main :: IO()
main = partOne