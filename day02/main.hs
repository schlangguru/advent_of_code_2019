import Computer
import StringUtil
import ListUtil

main :: IO ()
main = do
  input <- readFile "input.txt"
  let cmds = setAt 1 12 $
              setAt 2 2 $
              map (read::String->Int) $ split ',' input
  print $ process (cmds, cmds)
