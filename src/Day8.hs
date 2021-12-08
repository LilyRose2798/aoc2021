module Day8 (parse, solveOne, solveTwo) where

import Control.Monad (join)
import Data.Maybe (fromJust)
import Data.List (group, sort)
import Data.List.Extra (stripInfix)
import Data.Tuple.Extra (both)
import Data.Digits (unDigits)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

digitSegments :: [String]
digitSegments = ["abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"]

segmentsToFreqSumBy :: [String] -> String -> Int
segmentsToFreqSumBy = (\m -> sum . map (m Map.!)) . Map.fromList . zip ['a'..'g'] . map length . group . sort . join

freqSumMap :: Map.Map Int Int
freqSumMap = Map.fromList (zip (map (segmentsToFreqSumBy digitSegments) digitSegments) [0..])

parse :: String -> [([String], [String])]
parse = map (both words . fromJust . stripInfix " | ") . lines

solve :: ([String], [String]) -> [Int]
solve (xs, ys) = map ((freqSumMap Map.!) . segmentsToFreqSumBy xs) ys

solveOne :: [([String], [String])] -> Int
solveOne = length . filter (`Set.member` (Set.fromList [1, 4, 7, 8])) . (>>= solve)

solveTwo :: [([String], [String])] -> Int
solveTwo = sum . map (unDigits 10 . solve)
