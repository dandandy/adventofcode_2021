-- {-# LANGUInt DeriveAnyClass #-}
module Day6 (day6part1, day6part2) where

import Text.Parsec
import Text.Read

import Text.Parsec.String (parseFromFile)
import qualified Data.Map as Map
import Data.List

day6part1 :: IO ()
day6part1 = do
    x <- parseFromFile readInput "input6.txt"
    case x of
        Left pe -> error $ show pe
        Right m_ns -> print (sum . map snd . Map.toList . simulate 80 . inputToMap <$> m_ns)

day6part2 :: IO ()
day6part2 = do
    x <- parseFromFile readInput "input6.txt"
    case x of
        Left pe -> error $ show pe
        Right m_ns -> print (sum . map snd . Map.toList . simulate 256 . inputToMap  <$> m_ns)

example :: String
example = "3,4,3,1,2"

day :: Map.Map Int Int -> Map.Map Int Int
day = Map.foldrWithKey update (Map.fromList (zip [0..8] (repeat 0)))

update :: Int -> Int -> Map.Map Int Int -> Map.Map Int Int
update 0 value m = Map.update (\z -> (+z) <$> Just value) 6 (Map.update (\z -> (+z) <$> Just value) 8 m)
update k value m = Map.update (\z -> (+z) <$> Just value) (k - 1) m

simulate :: Int -> Map.Map Int Int -> Map.Map Int Int
simulate a n = iterate day n !! a

inputToMap :: [Int] -> Map.Map Int Int
inputToMap as = Map.fromList (countCopies as)

countCopies :: Eq a => [a] -> [(a, Int)]
countCopies xs = map (\a -> (a, length (filter (==a) xs)))  uniq
    where uniq = nub xs

readInput :: (Monad m) => ParsecT String u m (Maybe [Int])
readInput = mapM readMaybe <$> many1 digit `sepBy` char ','
