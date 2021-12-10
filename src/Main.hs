{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (catch, IOException)
import Control.Monad (zipWithM)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import Data.Conduit ((.|), runConduitRes)
import Data.Conduit.Combinators (sinkFile)
import Network.HTTP.Simple (parseRequest, httpSource, addRequestHeader, getResponseBody)
import System.TimeIt (timeItT)
import Text.Printf (printf)

import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09

solve :: (Show b, Show c) => (String -> a) -> (a -> b) -> (a -> c) -> String -> String
solve parse solveOne solveTwo = ((\x y -> "Part One: " ++ show x ++ "\nPart Two: " ++ show y) <$> solveOne <*> solveTwo) . parse

solvers =
  [ solve Day01.parse Day01.solveOne Day01.solveTwo
  , solve Day02.parse Day02.solveOne Day02.solveTwo
  , solve Day03.parse Day03.solveOne Day03.solveTwo
  , solve Day04.parse Day04.solveOne Day04.solveTwo
  , solve Day05.parse Day05.solveOne Day05.solveTwo
  , solve Day06.parse Day06.solveOne Day06.solveTwo
  , solve Day07.parse Day07.solveOne Day07.solveTwo
  , solve Day08.parse Day08.solveOne Day08.solveTwo
  , solve Day09.parse Day09.solveOne Day09.solveTwo
  ]

showDay :: Int -> String
showDay n | n < 10 = '0' : show n
          | otherwise = show n

downloadInput :: Int -> IO ()
downloadInput n = do
  req <- parseRequest ("GET https://adventofcode.com/2021/day/" ++ show n ++ "/input")
  cookie <- BS.readFile ".cookie"
  runConduitRes $ httpSource (addRequestHeader "Cookie" cookie req) getResponseBody .| sinkFile ("input/day" ++ showDay n ++ ".txt")

readInput :: Int -> IO String
readInput n = catch readFileN (const (downloadInput n >>= const readFileN) :: IOException -> IO String)
  where readFileN = readFile ("input/day" ++ showDay n ++ ".txt")

runDay :: (String -> String) -> Int -> IO Double
runDay s n = do
  i <- readInput n
  (t, _) <- timeItT $ putStrLn ("Day " ++ showDay n ++ "\n" ++ s i)
  printf "CPU Time: %.3fs\n\n" t
  return t

main :: IO ()
main = do
  ts <- zipWithM runDay solvers [1..]
  printf "Total CPU Time: %.3fs\n" (sum ts)
