module Day4 (parse, solveOne, solveTwo) where

import Data.List.Split (splitOn)
import Data.List (transpose, inits, find)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Set (Set)

type BingoLine = Set Int
type DrawSet = Set Int
type Draws = [Int]

parse :: String -> (Draws, [[BingoLine]])
parse = (\(n:bs) -> (map read $ splitOn "," n, map (map Set.fromList . ((++) <*> transpose) . map (map read . words) . lines) bs)) . splitOn "\n\n"

hasBingo :: DrawSet -> [BingoLine] -> Bool
hasBingo = any . flip Set.isSubsetOf

score :: Draws -> [BingoLine] -> Int
score ds bls = last ds * sum (Set.difference (Set.unions bls) (Set.fromList ds))

solveOne :: (Draws, [[BingoLine]]) -> Int
solveOne (ds, blss) = (score <*> (fromJust . flip find blss . hasBingo . Set.fromList)) $ fromJust $ find (flip any blss . hasBingo . Set.fromList) (inits ds)

rec :: [Draws] -> [[BingoLine]] -> Int
rec dss (bls:[]) = flip score bls $ fromJust $ find ((flip hasBingo) bls . Set.fromList) dss
rec (ds:dss) blss = rec dss (filter (not . hasBingo (Set.fromList ds)) blss)

solveTwo :: (Draws, [[BingoLine]]) -> Int
solveTwo = uncurry (rec . inits)
