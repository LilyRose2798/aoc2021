module Day01 (parse, solveOne, solveTwo) where

parse :: String -> [Int]
parse = map read . lines

solve :: Int -> [Int] -> Int
solve n = length . filter id . (zipWith (<) <*> drop n)

solveOne :: [Int] -> Int
solveOne = solve 1

solveTwo :: [Int] -> Int
solveTwo = solve 3
