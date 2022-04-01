module Day13 (day13part1, day13part2) where

import Control.Applicative (liftA2)
import Control.Monad (liftM)
import Control.Monad.Cont (lift)
import qualified Control.Monad.Identity as Monad
import Control.Monad.Trans.Maybe (MaybeT)
import qualified Data.Semigroup as Monad
import Text.Parsec
  ( Parsec,
    ParsecT,
    char,
    endBy,
    endOfLine,
    eof,
    parse,
    runParsecT,
    runParserT,
    sepBy,
    sepBy1,
    space,
    string,
    try,
    (<|>),
  )
import Text.Parsec.Char (digit)
import Text.Parsec.Combinator (many1)
import Text.Parsec.String (parseFromFile)
import Text.Read (readMaybe)

type Point = (Int, Int)

data Axis = X | Y deriving (Show)

data FoldAlong = FoldAlong Axis Int deriving (Show)

day13part1 :: IO ()
day13part1 = do
  x <- parseFromFile parseInput "example13.txt"
  print x

day13part2 :: Monad m => m ()
day13part2 = pure ()

parseInput :: Monad m => ParsecT String u m ([Point], [FoldAlong])
parseInput = (,) <$> (parsePoints <* endOfLine) <*> foldAlongLines <* eof

parseDigit :: Monad m => ParsecT String u m Int
parseDigit = read <$> many1 digit

comma :: Monad m => ParsecT String u m Char
comma = char ','

-- parsePoint :: ParsecT String u Maybe  (Int, Int) --(Int, Int)
parsePoint :: Monad m => ParsecT String u m Point
parsePoint = (,) <$> parseDigit <* comma <*> parseDigit

parsePoints :: Monad m => ParsecT String u m [Point]
parsePoints = parsePoint `endBy` endOfLine

foldAlongLines :: Monad m => ParsecT String u m [FoldAlong]
foldAlongLines = (try foldAlongX <|> try foldAlongY) `endBy` endOfLine

foldAlongX :: Monad m => ParsecT String u m FoldAlong
foldAlongX = FoldAlong X <$> (foldAlong *> space *> xEq 'x')

foldAlongY :: Monad m => ParsecT String u m FoldAlong
foldAlongY = FoldAlong Y <$> (foldAlong *> space *> xEq 'y')

foldAlong :: Monad m => ParsecT String u m String
foldAlong = (\a b c -> a ++ [b] ++ c) <$> string "fold" <*> char ' ' <*> string "along"

xEq :: Monad m => Char -> ParsecT String u m Int
xEq c = read <$> (char c *> char '=' *> many1 digit)
