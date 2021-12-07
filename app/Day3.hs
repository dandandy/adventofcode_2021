module Day3 (day3part1, day3part2) where

import Data.List


getFile = readFile "input3.txt"

example :: IO String
example = do return "00100 11110 10110 10111 10101 01111 00111 11100 10000 11001 00010 01010"

day3part1 = do
  x <- getFile
  putStrLn $ (("Day 3 Part 1: " ++) . show . multiplyTuple . applyToTuple binaryToInt . applyFunction notBinary . map mostCommon . transpose . parseInput) x

day3part2 = do
  x <- getFile
  putStrLn $ (("Day 3 Part 2: " ++) . show . multiplyTuple . applyTwoFunctions oxygenGeneratorRating co2ScrubberRating . parseInput) x

parseInput :: String -> [[Int]]
parseInput i = map (map readChar) $ words i

applyTwoFunctions :: (a -> b) -> (a -> c) -> a -> (b, c)
applyTwoFunctions f1 f2 v = (f1 v, f2 v)

readChar :: Char -> Int
readChar = read . pure

co2ScrubberRating :: [[Int]] -> Int
co2ScrubberRating ls = binaryToInt $ co2ScrubberRatingCriteria 0 ls

co2ScrubberRatingCriteria :: Int -> [[Int]] -> [Int]
co2ScrubberRatingCriteria _ [v] = v
co2ScrubberRatingCriteria n vs | length vs > 1 = co2ScrubberRatingCriteria (n + 1) $ filter (\xs -> (xs !! n) == leastCommon') vs
  where
    leastCommon' = if common n vs == 0 then 1 else 0
co2ScrubberRatingCriteria _ _ = []

oxygenGeneratorRating :: [[Int]] -> Int
oxygenGeneratorRating ls = binaryToInt $ oxygenBitCriteria 0 ls

oxygenBitCriteria :: Int -> [[Int]] -> [Int]
oxygenBitCriteria _ [v] = v
oxygenBitCriteria n vs
  | length vs > 1 = oxygenBitCriteria (n + 1) $ filter (\xs -> (xs !! n) == common') vs
  where
    common' = common n vs
oxygenBitCriteria _ _ = []

common :: Int -> [[Int]] -> Int
common n vs  = mostCommon $ map (!! n) vs

mostCommon :: [Int] -> Int
mostCommon l = case partition (== 1) l of
  (x, y) -> if length x >= length y then head x else head y

binaryToInt :: [Int] -> Int
binaryToInt xs = sum (zipWith (\x y -> x * 2 ^ y) (reverse xs) (iterate (+ 1) 0))

notBinary :: [Int] -> [Int]
notBinary = map (\x -> if x == 0 then 1 else 0)

applyFunction :: (a -> b) -> a -> (a, b)
applyFunction f a = (a, f a)

applyToTuple :: (a -> b) -> (a, a) -> (b, b)
applyToTuple f (a, b) = (f a, f b)

multiplyTuple :: Num a => (a, a) -> a
multiplyTuple (a, b) = a * b
