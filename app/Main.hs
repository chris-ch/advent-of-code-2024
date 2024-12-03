module Main where

import qualified Data.Map.Strict as Map
import Data.List (sort)

main :: IO ()
main = do
    content <- readFile "input2.txt"
    let rows = parseRows content
    let diffs = map makeDiff rows
    let isValidList = map isValid diffs
    let countValid = length $ filter id isValidList
    putStrLn $ "Number of valid rows: " ++ show countValid

parseRows :: String -> [[Int]]
parseRows content =
    let linesOfFile = lines content
    in map (map read . words) linesOfFile

isValid :: [Int] -> Bool
isValid xs = isValidIncreasing xs || isValidDecreasing xs

isValidIncreasing :: [Int] -> Bool
isValidIncreasing [] = True
isValidIncreasing (x:xs) = x `elem` [1, 2, 3] && isValidIncreasing xs

isValidDecreasing :: [Int] -> Bool
isValidDecreasing [] = True
isValidDecreasing (x:xs) = x `elem` [-1, -2, -3] && isValidDecreasing xs

makeDiff :: [Int] -> [Int]
makeDiff [] = []
makeDiff [_] = []
makeDiff (x:y:ys) = (y - x) : makeDiff ys

