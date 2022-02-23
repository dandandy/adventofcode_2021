
module Day9 (day9part1, day9part2) where

import qualified Data.Matrix as M
import Text.Parsec
import qualified Data.Maybe
import Control.Monad (filterM, join)
import Debug.Trace
import Text.Parsec.String (parseFromFile)
import Data.List (sort, nub)

day9part1 :: IO ()
day9part1 = do x <- parseFromFile parseInput "input9.txt"
               case M.fromLists <$> x of
                 Left pe -> error $ show pe
                 Right ma -> print $ sum <$> map (+1) <$>  getLowPoints ma

day9part2 :: IO ()
day9part2 = do x <- parseFromFile parseInput "input9.txt"
               case M.fromLists <$> x of
-- day9part2 = do case M.fromLists <$> parse parseInput "" example of
                 Left pe -> error $ show pe
                 Right ma -> print $ product <$> take 3 <$> reverse <$> sort <$> map length <$> map nub <$> map (\(x,y,z) -> findBasin x y ma) <$> getLowPointsIndex ma
                --  Right ma -> print ma

manyDigit :: Monad m =>  ParsecT String u m [Int]
manyDigit = many1 (read . pure <$> digit)

parseInput :: Monad m => ParsecT String u m [[Int]]
parseInput = manyDigit `sepBy` newline

checkIsPosMin :: (Ord a) => Int -> Int -> M.Matrix a -> Maybe a
checkIsPosMin a b m = do m <- min
                         if elem < m then return elem else Nothing
    where   min = minNeighbour a b m
            elem = M.getElem  a b m

getLowPoints :: M.Matrix Int -> Maybe [Int]
getLowPoints ma = sequence $ filter Data.Maybe.isJust $ [checkIsPosMin x y ma | x <- [1..(M.nrows ma)], y <- [1..(M.ncols ma)]]

getLowPointsIndex :: (Ord a) => M.Matrix a -> Maybe [(Int, Int, a)]
getLowPointsIndex ma = sequence $ filter Data.Maybe.isJust $ [(\s -> (x, y, s)) <$> checkIsPosMin x y ma | x <- [1..(M.nrows ma)], y <- [1..(M.ncols ma)]]

minNeighbour :: (Ord a) => Int -> Int -> M.Matrix a -> Maybe a
minNeighbour a b m = minimum <$> sequence (filter Data.Maybe.isJust (neighbours a b m))

neighbours :: Int -> Int -> M.Matrix a -> [Maybe a]
neighbours a b m =[M.safeGet rowsPlusOne b m, M.safeGet a colsPlusOne m, M.safeGet rowsMinusOne b m, M.safeGet a colsMinusOne m]
    where
            rowsPlusOne = a + 1
            rowsMinusOne = a - 1
            colsPlusOne = b + 1
            colsMinusOne = b - 1

neighboursIndex :: Int -> Int -> M.Matrix a -> [Maybe (Int, Int, a)]
neighboursIndex a b m =[safeGetWithIndex rowsPlusOne b m, safeGetWithIndex a colsPlusOne m, safeGetWithIndex rowsMinusOne b m, safeGetWithIndex a colsMinusOne m]
    where
            rowsPlusOne = a + 1
            rowsMinusOne = a - 1
            colsPlusOne = b + 1
            colsMinusOne = b - 1

safeGetWithIndex :: Int -> Int -> M.Matrix a -> Maybe (Int, Int, a)
safeGetWithIndex a b m = case M.safeGet a b m of
  Nothing -> Nothing
  Just a' -> Just (a, b, a')

findBasin :: (Ord a, Num a, Show a) => Int -> Int -> M.Matrix a -> [(Int, Int)]
findBasin a b m = (((a,b) :) . concatMap (\(x,y,z) -> findBasin x y m) . filter (\(x,y,z) -> z == elem + 1 && z /= 9)) ns
    where   ns = getListNeighbours a b m
            elem = M.getElem  a b m

getListNeighbours :: (Ord a, Num a, Show a) => Int -> Int -> M.Matrix a -> [(Int, Int, a)]
getListNeighbours a b m = Data.Maybe.fromMaybe [] $ sequence $ filter Data.Maybe.isJust $ neighboursIndex a b m

example :: [Char]
example = "2199943210\n\
\3987894921\n\
\9856789892\n\
\8767896789\n\
\9899965678"
