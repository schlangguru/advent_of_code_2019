module Common where

type Point = (Int, Int)

addPoint :: Point -> Point -> Point
addPoint (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

mannDist :: Point -> Point -> Int
mannDist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)