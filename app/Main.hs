module Main where

import qualified Data.Map.Strict as Map
import Data.List (sort)

main :: IO ()
main = do
    -- Read the file and parse the input into two arrays
    content <- readFile "input.txt"
    let (l1, l2) = parseInput content

    -- Sort both arrays
    let sortedL1 = sort l1
        sortedL2 = sort l2

    -- Compute the sum of absolute differences
    let sumAbsDiffs = sum $ zipWith (\v1 v2 -> abs (v1 - v2)) sortedL1 sortedL2
    putStrLn $ "Sum of absolute differences: " ++ show sumAbsDiffs

    -- Reparse the input for the second computation
    let (l1', l2') = parseInput content

    -- Compute frequencies of l2'
    let freqs = frequencyMap l2'

    -- Compute similarity score
    let similarity = sum $ map (\v -> v * Map.findWithDefault 0 v freqs) l1'
    putStrLn $ "Similarity score: " ++ show similarity

-- Parse the input file into two integer lists
parseInput :: String -> ([Int], [Int])
parseInput content =
    let linesOfFile = lines content
        numbers = map ((\[v1, v2] -> (read v1, read v2)) . words) linesOfFile
    in (map fst numbers, map snd numbers)

-- Create a frequency map from a list of integers
frequencyMap :: [Int] -> Map.Map Int Int
frequencyMap = foldr (\v m -> Map.insertWith (+) v 1 m) Map.empty
