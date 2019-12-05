import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Common
import InputParser
import Wire

partOne :: String -> IO()
partOne input = print $ Set.lookupMin $ Set.map (mannDist (0,0)) (Set.intersection wire1 wire2)
  where
    [wire1, wire2] = map (Set.fromList . buildWire . parseInput) $ lines input

main :: IO()
main = do
  input <- readFile "input.txt"
  partOne input
