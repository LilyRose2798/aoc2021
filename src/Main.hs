{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import Data.Conduit ((.|), runConduitRes)
import Data.Conduit.Combinators (sinkFile)
import Network.HTTP.Simple

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5

solve :: (Show b, Show c) => (String -> a) -> (a -> b) -> (a -> c) -> String -> String
solve parse solveOne solveTwo = ((\x y -> "Part One = " ++ show x ++ " / Part Two = " ++ show y) <$> solveOne <*> solveTwo) . parse

solvers = 
  [ solve Day1.parse Day1.solveOne Day1.solveTwo
  , solve Day2.parse Day2.solveOne Day2.solveTwo
  , solve Day3.parse Day3.solveOne Day3.solveTwo
  , solve Day4.parse Day4.solveOne Day4.solveTwo
  , solve Day5.parse Day5.solveOne Day5.solveTwo
  ]

downloadInput :: Int -> IO ()
downloadInput n = do
  req <- parseRequest ("GET https://adventofcode.com/2021/day/" ++ show n ++ "/input")
  cookie <- BS.readFile ".cookie"
  runConduitRes $ httpSource (addRequestHeader "Cookie" cookie req) getResponseBody .| sinkFile ("input/day" ++ show n ++ ".txt")

readInput :: Int -> IO String
readInput n = catch (readFile ("input/day" ++ show n ++ ".txt")) (\e -> const (downloadInput n >>= const (readInput n)) (e :: IOException))

solveAll :: IO String
solveAll = fmap unlines $ mapM (\(s, n) -> ((("Day " ++ show n ++ ": ") ++) . s) <$> readInput n) $ zip solvers [1..]

main :: IO ()
main = solveAll >>= putStrLn
