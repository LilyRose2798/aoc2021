module Day6 (parse, solveOne, solveTwo) where

import Data.List.Split (splitOn)
import Data.List (group, sort)

parse :: String -> [Int]
parse = map (subtract 1 . length) . group . sort . (++ [0..8]) . map read . splitOn ","

simulate :: [Int] -> [Int]
simulate (n:ns) = take 6 ns ++ (\(n':ns') -> (n + n' : ns')) (drop 6 ns) ++ [n]

popAt :: Int -> [Int] -> Int
popAt n = sum . (!! n) . iterate simulate

solveOne :: [Int] -> Int
solveOne = popAt 80

solveTwo :: [Int] -> Int
solveTwo = popAt 256
