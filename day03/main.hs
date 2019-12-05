import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Common
import InputParser
import Wire

partOne :: String -> IO()
partOne input = print $ Set.lookupMin $ Set.map (mannDist (0,0)) (Set.intersection (Set.fromList wire1) (Set.fromList wire2))
  where
    [wire1, wire2] = map (buildWire . parseInput) $ lines input

partTwo :: String -> IO()
partTwo input = let intersection = Set.intersection (Set.fromList $ wires!!0) (Set.fromList $ wires!!1) in
  print $ Set.lookupMin $ Set.map (\point -> combindedStepsTo point wires) intersection
  where
    wires = map (buildWire . parseInput) $ lines input

main :: IO()
main = do
  input <- readFile "input.txt"
  partTwo input
