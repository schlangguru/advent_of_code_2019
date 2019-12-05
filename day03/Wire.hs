module Wire where

import Common

type Wire = [Point]

buildWire :: [Point] -> Wire
buildWire points = scanl1 addPoint points


