module Day03 (parse, solveOne, solveTwo) where

import Data.List (transpose, partition)
import Data.Function (on)
import Util.Bits (fromListBE)

parse :: String -> [[Bool]]
parse = map (map (== '1')) . lines

hasMoreFalses :: [Bool] -> Bool
hasMoreFalses = uncurry ((<) `on` length) . partition id

mulBools :: [Bool] -> [Bool] -> Int
mulBools = (*) `on` fromListBE

solveOne :: [[Bool]] -> Int
solveOne = (mulBools <*> map not) . map hasMoreFalses . transpose

rec :: Bool -> [Bool] -> [[Bool]] -> [Bool]
rec _ a ([]:_) = reverse a
rec _ a [c] = reverse a ++ c
rec i a cs = (rec i <$> (: a) <*> (\x -> map tail $ filter ((== x) . head) cs)) ((== i) $ hasMoreFalses $ map head cs)

solveTwo :: [[Bool]] -> Int
solveTwo = mulBools <$> rec True [] <*> rec False []
