{-# OPTIONS -Wno-incomplete-patterns #-}

module Day10 (Day10.parse, solveOne, solveTwo) where

import Text.Parsec as Parsec (ParseError, char, between, choice, eof, many, parse, ParsecT)
import Text.Parsec.Error (errorMessages, messageString)
import Control.Monad (void)
import Control.Applicative (liftA2)
import Data.List (find, sort)
import Data.Maybe (isNothing)

parse :: String -> [String]
parse = lines

chunks :: Monad m => ParsecT String u m ()
chunks = void (many chunk)
  where chunk = choice
          [ between (char '(') (char ')') chunks
          , between (char '[') (char ']') chunks
          , between (char '{') (char '}') chunks
          , between (char '<') (char '>') chunks
          ]

getChunksError :: String -> Maybe (Maybe Char, Maybe Char)
getChunksError = either (Just . liftA2 (,) last head . map (find (/= '"') . messageString) . errorMessages) (const Nothing) . Parsec.parse (chunks >> eof) ""

scoreOne :: Char -> Int
scoreOne ')' = 3
scoreOne ']' = 57
scoreOne '}' = 1197
scoreOne '>' = 25137

solveOne :: [String] -> Int
solveOne = sum . map (maybe 0 (maybe 0 scoreOne . snd) . getChunksError)

scoreTwo :: Char -> Int
scoreTwo ')' = 1
scoreTwo ']' = 2
scoreTwo '}' = 3
scoreTwo '>' = 4

completedScores :: String -> [Int]
completedScores x = maybe [] (maybe [] (\c -> scoreTwo c : completedScores (x ++ [c])) . fst) (getChunksError x)

solveTwo :: [String] -> Int
solveTwo = ((!!) <*> (`div` 2) . length) . sort . map (foldl ((+) . (* 5)) 0 . completedScores) . filter (maybe False (isNothing . snd) . getChunksError)
