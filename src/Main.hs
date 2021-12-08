{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (catch, IOException)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import Data.Conduit ((.|), runConduitRes)
import Data.Conduit.Combinators (sinkFile)
import Network.HTTP.Simple (parseRequest, httpSource, addRequestHeader, getResponseBody)
import System.TimeIt (timeItT)
import Text.Printf (printf)

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8

solve :: (Show b, Show c) => (String -> a) -> (a -> b) -> (a -> c) -> String -> String
solve parse solveOne solveTwo = ((\x y -> "Part One: " ++ show x ++ "\nPart Two: " ++ show y) <$> solveOne <*> solveTwo) . parse

solvers = 
  [ solve Day1.parse Day1.solveOne Day1.solveTwo
  , solve Day2.parse Day2.solveOne Day2.solveTwo
  , solve Day3.parse Day3.solveOne Day3.solveTwo
  , solve Day4.parse Day4.solveOne Day4.solveTwo
  , solve Day5.parse Day5.solveOne Day5.solveTwo
  , solve Day6.parse Day6.solveOne Day6.solveTwo
  , solve Day7.parse Day7.solveOne Day7.solveTwo
  , solve Day8.parse Day8.solveOne Day8.solveTwo
  ]

downloadInput :: Int -> IO ()
downloadInput n = do
  req <- parseRequest ("GET https://adventofcode.com/2021/day/" ++ show n ++ "/input")
  cookie <- BS.readFile ".cookie"
  runConduitRes $ httpSource (addRequestHeader "Cookie" cookie req) getResponseBody .| sinkFile ("input/day" ++ show n ++ ".txt")

readInput :: Int -> IO String
readInput n = catch readFileN (const (downloadInput n >>= const readFileN) :: IOException -> IO String)
  where readFileN = readFile ("input/day" ++ show n ++ ".txt")

runDay :: (String -> String) -> Int -> IO ()
runDay s n = do
  i <- readInput n
  (t, _) <- timeItT $ putStrLn ("Day " ++ show n ++ "\n" ++ s i)
  printf "CPU Time: %.3fs\n\n" t

main :: IO ()
main = do
  (t, _) <- timeItT $ mapM_ (uncurry runDay) (zip solvers [1..])
  printf "Total CPU Time: %.3fs\n" t
