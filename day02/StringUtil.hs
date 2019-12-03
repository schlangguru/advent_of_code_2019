module StringUtil (split) where

split :: Char -> String -> [String]
split _ [] = []
split delim str = el : split delim rs
  where (el, rs) = extractUntil delim str

extractUntil :: Char -> String -> (String, String)
extractUntil delim str = (el, dropWhile (== delim) rs)
  where (el, rs) = span (/= delim) str