module Rules where

import Data.List

hasSixDigits :: Int -> Bool
hasSixDigits nmb = length digits == 6
  where digits = show nmb

digitsDontDecrease :: Int -> Bool
digitsDontDecrease nmb = digits == sort digits
  where
    digits = show nmb

hasMultiDigit :: Int -> Bool
hasMultiDigit nmb = hasMultiDigit' digits
  where
    digits = show nmb
    hasMultiDigit' (a:b:xs) = a == b || hasMultiDigit' (b : xs)
    hasMultiDigit' _  = False

hasDoubleDigit :: Int -> Bool
hasDoubleDigit nmb = any ((==2) . length) (group digits)
  where
    digits = show nmb