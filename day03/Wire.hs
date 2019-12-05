module Wire where

import Common
import Data.List
import Data.Maybe

type Wire = [Point]

buildWire :: [Point] -> Wire
buildWire points = scanl1 addPoint points

stepsTo :: Point -> Wire -> Int
stepsTo point wire = (fromJust $ elemIndex point wire) + 1

combindedStepsTo :: Point -> [Wire] -> Int
combindedStepsTo point wires = foldl1 (+) $ map (stepsTo point) wires