import Rules

partTwo :: [Int] -> IO()
partTwo range = print
                $ length
                $ filter (\x -> all ($ x) [hasSixDigits, digitsDontDecrease, hasDoubleDigit]) range

partOne :: [Int] -> IO()
partOne range = print
          $ length
          $ filter (\x -> all ($ x) [hasSixDigits, digitsDontDecrease, hasMultiDigit]) range

main :: IO()
main = do
  let range = [147981..691423]
  partTwo range