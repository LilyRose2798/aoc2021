module Day8 (parse, solveOne, solveTwo) where

import Control.Monad (join)
import Data.Maybe (fromJust)
import Data.List (group, sort)
import Data.List.Extra (stripInfix)
import Data.Tuple.Extra (both)
import Data.Digits (unDigits)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

freqSumMap :: Map.Map Int Int
freqSumMap = Map.fromList (zip [42, 17, 34, 39, 30, 37, 41, 25, 49, 45] [0..])

parse :: String -> [([String], [String])]
parse = map (both words . fromJust . stripInfix " | ") . lines

solve :: ([String], [String]) -> [Int]
solve (xs, ys) = map ((freqSumMap Map.!) . sum . map (charFreqMap Map.!)) ys
  where charFreqMap = Map.fromList $ zip ['a'..'g'] $ map length $ group $ sort $ join xs

solveOne :: [([String], [String])] -> Int
solveOne = length . filter (`Set.member` wantedNums) . (>>= solve)
  where wantedNums = Set.fromList [1, 4, 7, 8]

solveTwo :: [([String], [String])] -> Int
solveTwo = sum . map (unDigits 10 . solve)
