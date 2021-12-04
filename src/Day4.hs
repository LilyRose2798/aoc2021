module Day4 (parse, solveOne, solveTwo) where

import Data.List.Split (splitOn)
import Data.List (transpose, inits, find)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Set (Set)

type BingoLine = Set Int
type DrawSet = Set Int
type Draws = [Int]

parse :: String -> ([[BingoLine]], Draws)
parse = (\(n:bs) -> (map (map Set.fromList . ((++) <*> transpose) . map (map read . words) . lines) bs, map read $ splitOn "," n)) . splitOn "\n\n"

hasBingo :: DrawSet -> [BingoLine] -> Bool
hasBingo = any . flip Set.isSubsetOf

score :: [BingoLine] -> Draws -> Int
score bls ds = last ds * sum (Set.difference (Set.unions bls) (Set.fromList ds))

solveOne :: ([[BingoLine]], Draws) -> Int
solveOne (blss, ds) = (flip score <*> (fromJust . flip find blss . hasBingo . Set.fromList)) $ fromJust $ find (flip any blss . hasBingo . Set.fromList) (inits ds)

rec :: [[BingoLine]] -> [Draws] -> Int
rec (bls:[]) dss = score bls $ fromJust $ find ((flip hasBingo) bls . Set.fromList) dss
rec blss (ds:dss) = rec (filter (not . hasBingo (Set.fromList ds)) blss) dss

solveTwo :: ([[BingoLine]], Draws) -> Int
solveTwo (blss, ds) = rec blss (inits ds)
