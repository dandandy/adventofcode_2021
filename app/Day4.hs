{-# LANGUAGE FlexibleContexts #-}
module Day4 (day4part1, day4part2) where

-- import Text.ParserCombinators.Parsec
import Text.Parsec

import Text.Read (readMaybe)
import Debug.Trace
import Data.Char (isSpace)

-- test :: Stream s Data.Functor.Identity.Identity t => Parsec s () a -> s -> Either ParseError a
-- test p = parse p ""

day4part1 :: IO ()
day4part1 = do x <- example
               print $  parse game "input" x

day4part2 :: IO ()
day4part2 = pure ()

type Board = [[Square]]

data Square = Square Bool Int deriving (Show)

data Game = Game [Int] Board deriving (Show)

comma :: Monad m => ParsecT String u m Char
comma = char ','

number :: Monad m => ParsecT String u m Int
number = read <$> many1 digit
-- number = (\a -> trace (show a) read a) <$> many digit

draws :: Monad m => ParsecT String u m [Int]
draws = try $ number `sepBy` comma -- `endBy` endOfLine

-- paddedNumber :: Monad m => ParsecT String u m Int
-- paddedNumber = space *> number <* skipMany space

boardRow :: Monad m => ParsecT String u m [Square]
boardRow = map (Square False) <$> many (many (char ' ') *> number <* many (char ' ')) <* char '\n'
-- boardRow = map (Square False) <$> (( number `sepEndBy` skipMany (oneOf " ")) <* newline )

game :: Monad m => ParsecT String u m Game
game = Game <$> (draws <* newline) <*> board

-- board :: Monad m => ParsecT String u m Board
board :: Monad m =>  ParsecT String u m [[Square]]
-- board = many (boardRow <* char '\n')
board = spaces *> many boardRow <* spaces

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


