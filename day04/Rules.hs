module Rules where

import Data.List

meetsRules :: Int -> Bool
meetsRules nmb = all ($ nmb) [hasSixDigits, digitsDontDecrease, hasDoubleDigit]

hasSixDigits :: Int -> Bool
hasSixDigits nmb = length digits == 6
  where digits = show nmb

digitsDontDecrease :: Int -> Bool
digitsDontDecrease nmb = digits == sort digits
  where
    digits = show nmb

hasDoubleDigit :: Int -> Bool
hasDoubleDigit nmb = hasDoubleDigit' digits
    where
      digits = show nmb
      hasDoubleDigit' (a:b:xs) = a == b || hasDoubleDigit' (b : xs)
      hasDoubleDigit' _  = False
