{-# LANGUAGE TupleSections #-}

module Day5 (parse, solveOne, solveTwo) where

import Data.Maybe (fromJust)
import Data.List.Extra (stripInfix, group, sort)
import Data.Tuple.Extra (both)

type Point = (Int, Int)
type Line = (Point, Point)

parse :: String -> [Line]
parse = map (both (both read . fromJust . stripInfix ",") . fromJust . stripInfix " -> ") . lines

(.:.) :: (Ord a, Enum a) => a -> a -> [a]
(.:.) a b = if a <= b then [a..b] else reverse [b..a]

toPoints :: Bool -> Line -> [Point]
toPoints diag ((x, y), (x', y'))
  | x == x' = map (x ,) (y .:. y')
  | y == y' = map (, y) (x .:. x')
  | diag = zip (x .:. x') (y .:. y')
  | otherwise = []

numIntersects :: Bool -> [Line] -> Int
numIntersects diag = length . filter ((>= 2) . length) . group . sort . (>>= toPoints diag)

solveOne :: [Line] -> Int
solveOne = numIntersects False

solveTwo :: [Line] -> Int
solveTwo = numIntersects True
