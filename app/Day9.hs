module Day9 (day9part1, day9part2) where

import qualified Data.Matrix as M
import Text.Parsec
import qualified GHC.OverloadedLabels as M

day9part1 :: IO ()
day9part1 = do print $ run 1 1 <$>  M.fromLists <$>  parse parseInput "" example
day9part2 :: IO ()
day9part2 = pure ()

manyDigit :: Monad m =>  ParsecT String u m [Int]
manyDigit = many1 (read . pure <$> digit)

parseInput :: Monad m => ParsecT String u m [[Int]]
parseInput = manyDigit `sepBy` newline

run :: (Num a, Ord a) => Int -> Int -> M.Matrix a -> Maybe a
run a b m = minimum <$> sequence [M.safeGet a b m, M.safeGet ((a + 1) `mod` M.nrows m) b m, M.safeGet a ((b + 1) `mod` M.ncols m) m, M.safeGet (a+1) ((b + 1) `mod` M.ncols m) m]


example :: [Char]
example = "2199943210\n\
\3987894921\n\
\9856789892\n\
\8767896789\n\
\9899965678"
