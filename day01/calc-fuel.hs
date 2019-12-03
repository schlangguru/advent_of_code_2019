calcFuel :: Int -> Int
calcFuel x
  | x <= 8      = 0
  | otherwise   = fuel + calcFuel fuel
  where fuel = x `quot` 3 - 2

main :: IO ()
main = do
  input <- readFile "input.txt"
  let modules = map (read::String->Int) (lines input)
  print $ sum $ map calcFuel modules
