module Day02 (parse, solveOne, solveTwo) where

import Control.Arrow ((***))
import Data.Maybe (fromJust)
import Data.List.Extra (stripInfix)

data Dir = Forward | Up | Down
data Command = Command Dir Int
data StateOne = StateOne Int Int -- x y
data StateTwo = StateTwo Int Int Int -- x y aim

readDir :: String -> Dir
readDir "forward" = Forward
readDir "up" = Up
readDir "down" = Down
readDir x = error $ "Unknown command " ++ x

parse :: String -> [Command]
parse = map (uncurry Command . (readDir *** read) . fromJust . stripInfix " ") . lines


moveOne :: StateOne -> Command -> StateOne
moveOne (StateOne x y) (Command Forward n) = StateOne (x + n) y
moveOne (StateOne x y) (Command Up n) = StateOne x (y - n)
moveOne (StateOne x y) (Command Down n) = StateOne x (y + n)

totalOne :: StateOne -> Int
totalOne (StateOne x y) = x * y

solveOne :: [Command] -> Int
solveOne = totalOne . foldl moveOne (StateOne 0 0)


moveTwo :: StateTwo -> Command -> StateTwo
moveTwo (StateTwo x y aim) (Command Forward n) = StateTwo (x + n) (y + (aim * n)) aim
moveTwo (StateTwo x y aim) (Command Up n) = StateTwo x y (aim - n)
moveTwo (StateTwo x y aim) (Command Down n) = StateTwo x y (aim + n)

totalTwo :: StateTwo -> Int
totalTwo (StateTwo x y _) = x * y

solveTwo :: [Command] -> Int
solveTwo = totalTwo . foldl moveTwo (StateTwo 0 0 0)
