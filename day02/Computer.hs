module Computer (process) where

import ListUtil

process :: ([Int], [Int]) -> [Int]
process ([], l)    = l
process (99:rs, l) = l
process (1:x:y:z:rs, l) = let result = l!!x + l!!y in process (rs, setAt z result l)
process (2:x:y:z:rs, l) = let result = l!!x * l!!y in process (rs, setAt z result l)