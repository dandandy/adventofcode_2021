{-# LANGUAGE FlexibleContexts #-}
module Day4 (day4part1, day4part2) where

-- import Text.ParserCombinators.Parsec
import Text.Parsec

import Text.Read (readMaybe)

import Data.Char (isSpace)
import Data.List
import Control.Arrow
import Data.Foldable (foldrM)
import Text.Parsec.String

day4part1 :: IO ()
day4part1 = do  x <- parseFromFile game "input4.txt"
                putStrLn ("Day 4 Part 1: "++ show ((\(a, b) -> getAllUnmarkedSum a * b) . play <$> x))

day4part2 :: IO ()
day4part2 = do  x <- parseFromFile game "input4.txt"
                putStrLn ("Day 4 Part 2: "++ show ((\(a, b) -> getAllUnmarkedSum a * b) . playToLose <$> x))

type Board = [[Square]]

data Square = Square Bool Int deriving (Show, Eq)

data Game = Game [Int] [Board] deriving (Show)

comma :: Monad m => ParsecT String u m Char
comma = char ','

number :: Monad m => ParsecT String u m Int
number = read <$> many1 digit

draws :: Monad m => ParsecT String u m [Int]
draws = try $ number `sepBy` comma

boardRow :: Monad m => ParsecT String u m [Square]
boardRow = map (Square False) <$> ( many (char ' ') *> (number `sepBy` many1 (char ' ')))


game :: Monad m => ParsecT String u m Game
game = Game <$> (draws <* newline)  <*> (newline *> boards)

boards :: Monad m =>  ParsecT String u m [Board]
boards = board `sepBy` newline

board :: Monad m =>  ParsecT String u m Board
board = (\a b c d e -> [a, b, c, d, e]) <$> (boardRow <* newline) <*> (boardRow <* newline) <*> (boardRow <* newline) <*> (boardRow <* newline) <*> (boardRow <* newline)

move :: Int -> Board -> Board
move n = map (map (\(Square b' i) -> if i == n then Square True i else Square b' i ))

getAllUnmarkedSum :: Board -> Int
getAllUnmarkedSum bs = sum $ map (\(Square _ i) -> i) (filter (\(Square b i) -> not b) (concat bs))

play :: Game -> (Board, Int)
play (Game (a:as) boards) = case applyMove a boards of
    bs -> case getWinner bs of
        Nothing -> play (Game as bs)
        Just winner -> (winner, a)
play (Game [] b) = ([], 0)

playToLose :: Game -> (Board, Int)
playToLose (Game (a:as) [board]) = case applyMove a [board] of
    bs -> case getWinners bs of
        [] -> playToLose (Game as bs)
        [winner] -> (winner, a)
        winners -> ([], 1)
playToLose (Game (a:as) boards) = case applyMove a boards of
    bs -> case getWinners bs of
        [] -> playToLose (Game as bs)
        winners -> playToLose (Game as (bs \\ winners))
playToLose (Game i _) = ([], 0)

getWinner :: [Board] -> Maybe Board
getWinner = find isWin

getWinners :: [Board] -> [Board]
getWinners = filter isWin

applyMove :: Int -> [Board] -> [Board]
applyMove i = map (move i)

isWin :: Board -> Bool
isWin b = colWin || rowWin b
    where   colWin = rowWin $ transpose b
            rowWin c = any rowAllTrue c
            rowAllTrue = all (\(Square b v) -> b)

example :: IO String
example = pure "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\
\\n\
\22 13 17 11  0\n\
\ 8  2 23  4 24\n\
\21  9 14 16  7\n\
\ 6 10  3 18  5\n\
\ 1 12 20 15 19\n\
\\n\
\ 3 15  0  2 22\n\
\ 9 18 13 17  5\n\
\19  8  7 25 23\n\
\20 11 10 24  4\n\
\14 21 16 12  6\n\
\\n\
\14 21 17 24  4\n\
\10 16 15  9 19\n\
\18  8 23 26 20\n\
\22 11 13  6  5\n\
\ 2  0 12  3  7\n\
\"


