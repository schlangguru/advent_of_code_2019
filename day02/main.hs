import StringUtil
import ListUtil

process :: ([Int], [Int]) -> [Int]
process ([], l)    = l
process (99:rs, l) = l
process (1:x:y:z:rs, l) = let result = l!!x + l!!y in process (rs, setAt z result l)
process (2:x:y:z:rs, l) = let result = l!!x * l!!y in process (rs, setAt z result l)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let cmds = setAt 1 12 (setAt 2 2 (map (read::String->Int) $ split ',' input))
  print $ process (cmds, cmds)
