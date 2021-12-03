module Day2 (parse, solveOne, solveTwo) where

data Command = Forward Int | Up Int | Down Int
data StateOne = StateOne Int Int -- x y
data StateTwo = StateTwo Int Int Int -- x y aim

readCommand :: String -> Int -> Command
readCommand "forward" = Forward
readCommand "up" = Up
readCommand "down" = Down
readCommand x = error $ "Unknown command " ++ x

parse :: String -> [Command]
parse = map ((\(x, y) -> readCommand x (read y)) . break (== ' ')) . lines


moveOne :: StateOne -> Command -> StateOne
moveOne (StateOne x y) (Forward n) = StateOne (x + n) y
moveOne (StateOne x y) (Up n) = StateOne x (y - n)
moveOne (StateOne x y) (Down n) = StateOne x (y + n)

totalOne :: StateOne -> Int
totalOne (StateOne x y) = x * y

solveOne :: [Command] -> Int
solveOne = totalOne . foldl moveOne (StateOne 0 0)


moveTwo :: StateTwo -> Command -> StateTwo
moveTwo (StateTwo x y aim) (Forward n) = StateTwo (x + n) (y + (aim * n)) aim
moveTwo (StateTwo x y aim) (Up n) = StateTwo x y (aim - n)
moveTwo (StateTwo x y aim) (Down n) = StateTwo x y (aim + n)

totalTwo :: StateTwo -> Int
totalTwo (StateTwo x y _) = x * y

solveTwo :: [Command] -> Int
solveTwo = totalTwo . foldl moveTwo (StateTwo 0 0 0)
