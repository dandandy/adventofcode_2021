module Day1 (day1part1, day1part2) where

import Text.Read (readMaybe)
import Text.Show (Show)

data Report = Increased | Decreased | NoChange deriving (Show, Eq)

example :: Maybe [Int]
example = toList "199 200 208 210 200 207 240 269 260 263"

readInputFile :: IO (Maybe [Int])
readInputFile = do
    x <- readFile "input1.txt"
    return $ toList x

day1part1 = do
    x <- readInputFile
    putStrLn $ "Day 1 Part 1: " ++ (show $ countIncreased . measure <$> x)

day1part2 = do
    x <- readInputFile
    putStrLn $ "Day 1 Part 2: " ++ (show $ countIncreased . measure . rollingWindow <$> x)

toList :: String -> Maybe [Int]
toList s = mapM readMaybe ( words s)

measure :: [Int] -> [Report]
measure (x:x':xs) = comparison x x' : measure (x':xs)
    where comparison x y
            | x == y = NoChange
            | x < y = Increased
            | x > y = Decreased
            | otherwise = NoChange
measure [x] = []
measure [] = []

countIncreased :: [Report] -> Int
countIncreased = foldl (\b a -> if a == Increased then b + 1 else b) 0

rollingWindow :: [Int] -> [Int]
rollingWindow (x:x':x'':xs) = x + x' + x'' : rollingWindow (x':x'':xs)
rollingWindow _ = []
