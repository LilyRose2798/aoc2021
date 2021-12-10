module Day09 (parse, solveOne, solveTwo) where

import Data.Char (digitToInt)
import Data.Maybe (mapMaybe)
import Data.List (sortOn)
import Data.Ord (Down(Down))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq

type Point = (Int, Int)

parse :: String -> Map.Map Point Int
parse = (\rows -> Map.fromList [((r, c), digitToInt n) | (r, row) <- zip [0..] rows, (c, n) <- zip [0..] row]) . lines

neighbours :: Point -> [Point]
neighbours (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

solveOne :: Map.Map Point Int -> Int
solveOne grid = sum $ Map.map (+ 1) $ Map.filterWithKey (\p n -> all (> n) (mapMaybe (grid Map.!?) (neighbours p))) grid

bfs :: (Ord a, Eq a) => (a -> Seq.Seq a) -> a -> [a]
bfs step start = rec Set.empty (Seq.singleton start)
  where rec seen Seq.Empty = []
        rec seen (cur Seq.:<| rest)
          | cur `Set.member` seen = rec seen rest
          | otherwise = cur : rec (Set.insert cur seen) (rest Seq.>< step cur)

basin :: Map.Map Point Int -> Point -> [Point]
basin grid = bfs (\p -> Seq.fromList $ filter
  (\p' -> case grid Map.!? p' of
    Nothing -> False
    Just n -> n /= 9 && n > (grid Map.! p)
  ) $ neighbours p)

solveTwo :: Map.Map Point Int -> Int
solveTwo grid = product $ take 3 $ sortOn Down $ map (Set.size . Set.fromList . basin grid) $ Map.keys grid
