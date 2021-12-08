{-# OPTIONS -Wno-incomplete-patterns #-}

module Day4 (parse, solveOne, solveTwo) where

import Control.Applicative (liftA2)
import Control.Arrow ((***))
import Data.List.Split (splitOn)
import Data.List (transpose, inits, find)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Set (Set)

type BingoLine = Set Int
type Board = [BingoLine]
type DrawSet = Set Int
type Draws = [Int]

parse :: String -> (Draws, [Board])
parse = (map read . splitOn "," *** map (map Set.fromList . ((++) <*> transpose) . map (map read . words) . lines)) . liftA2 (,) head tail . splitOn "\n\n"

hasBingo :: DrawSet -> Board -> Bool
hasBingo = any . flip Set.isSubsetOf

score :: Draws -> Board -> Int
score ds b = last ds * sum (Set.difference (Set.unions b) (Set.fromList ds))

solveOne :: (Draws, [Board]) -> Int
solveOne (ds, bs) = (score <*> (fromJust . flip find bs . hasBingo . Set.fromList)) $ fromJust $ find (flip any bs . hasBingo . Set.fromList) (inits ds)

rec :: [Draws] -> [Board] -> Int
rec dss (b:[]) = flip score b $ fromJust $ find (flip hasBingo b . Set.fromList) dss
rec (ds:dss) bs = rec dss (filter (not . hasBingo (Set.fromList ds)) bs)

solveTwo :: (Draws, [Board]) -> Int
solveTwo = uncurry (rec . inits)
