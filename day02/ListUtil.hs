module ListUtil (setAt) where

setAt :: Int -> a -> [a] -> [a]
setAt i el list
  | i >= length list     = list
  | i - 1 == length list = init list ++ [el]
  | i == 0               = el : tail list
  | otherwise            = let (h,_:t) = splitAt i list in h ++ [el] ++ t