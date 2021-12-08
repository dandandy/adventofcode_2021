module Day7 (day7part1, day7part2) where

import Text.Parsec
import Text.Read
import Text.Parsec.String

day7part1 :: IO ()
day7part1 =  do x <- parseFromFile parseInput "input7.txt"
    -- do case parse parseInput "" example of
                case x of
                 Left pe -> error $ show pe
                 Right m_ns -> print $ show (minimum .  allPossibleMoveCosts <$> m_ns)

day7part2 :: IO ()
day7part2 =  do x <- parseFromFile parseInput "input7.txt"
    -- do case parse parseInput "" example of
                case x of
                 Left pe -> error $ show pe
                 Right m_ns -> print $ show (minimum .  allPossibleMoveCostsPart2 <$> m_ns)

parseInput :: (Monad m) => ParsecT String u m (Maybe [Int])
parseInput = mapM readMaybe <$> many1 digit `sepBy` char ','

example = "16,1,2,0,4,2,7,1,2,14"
-- findOverlapping :: 

-- range :: Ord a => [a] -> [a]
allPossibleMoveCosts :: (Num a, Ord a, Enum a) => [a] -> [a]
allPossibleMoveCosts as = map (fuelToMoveToTarget as) (range as)

allPossibleMoveCostsPart2 :: (Num a, Ord a, Enum a) => [a] -> [a]
allPossibleMoveCostsPart2 as = map (fuelToMoveToTargetNonLinear as) (range as)


range :: (Foldable t, Ord a, Enum a) => t a -> [a]
range as = [(minimum as) .. (maximum as)]

fuelToMoveToTarget :: Num a => [a] -> a -> a
fuelToMoveToTarget as target = sum [abs (target - a) | a <- as]

fuelToMoveToTargetNonLinear :: (Num a, Ord a, Enum a) => [a] -> a -> a
fuelToMoveToTargetNonLinear as target = sum [sum (range [0, abs (target - a)]) | a <- as]
