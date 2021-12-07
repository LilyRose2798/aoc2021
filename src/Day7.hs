module Day7 (parse, solveOne, solveTwo) where

import Data.List.Split (splitOn)

parse :: String -> [Int]
parse = map read . splitOn ","

minFuelBy :: (Int -> Int) -> [Int] -> Int
minFuelBy f xs = minimum $ map (\x -> sum $ map (f . abs . subtract x) xs) [minimum xs..maximum xs]

solveOne :: [Int] -> Int
solveOne = minFuelBy id

solveTwo :: [Int] -> Int
solveTwo = minFuelBy (\x -> x * (x + 1) `div` 2)
