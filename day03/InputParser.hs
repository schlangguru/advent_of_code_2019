module InputParser where

import Common
import Data.List.Split

parseInput :: String -> [Point]
parseInput input = concatMap parseCmd cmds
  where cmds = splitOn "," input

parseCmd :: String -> [Point]
parseCmd (d:n) = replicate (read n :: Int) $ case d of
  'R' -> (1, 0)
  'L' -> (-1, 0)
  'U' -> (0, 1)
  'D' -> (0, -1)
  _   -> (0, 0)
parseCmd _     = []