module Main where

main :: IO ()
main = do
    content <- readFile "input2.txt"
    let rows = parseRows content
    let isValidList = map isValid rows
    let countValid = length $ filter id isValidList
    putStrLn $ "Number of valid rows: " ++ show countValid

parseRows :: String -> [[Int]]
parseRows content =
    let linesOfFile = lines content
    in map (map read . words) linesOfFile

isValid :: [Int] -> Bool
isValid xs = validWithRelaxation diffs [1, 2, 3] || validWithRelaxation diffs [-1, -2, -3]
  where
    diffs = makeDiff xs

validWithRelaxation :: [Int] -> [Int] -> Bool
validWithRelaxation diffs validRange = length (filter (`notElem` validRange) diffs) <= 1

makeDiff :: [Int] -> [Int]
makeDiff [] = []
makeDiff [_] = []
makeDiff (x:y:ys) = (y - x) : makeDiff (y:ys)
