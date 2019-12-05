import Rules

partOne :: IO()
partOne = print $ length $ filter meetsRules [147981..691423]

main :: IO()
main = partOne