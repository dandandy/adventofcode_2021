module Day12  (day12part1, day12part2) where
import qualified Control.Monad as Monad
import Text.Parsec (Parsec, many1, (<|>), sepEndBy)
import Text.Parsec.Char (letter)
import Text.Parsec (char, newline)
import Text.Parsec.Combinator (sepBy)
import Text.Parsec.Prim (parse)

day12part1 :: IO ()
day12part1 = pure (parse parseInput "" example) >>= print 

day12part2 :: Monad.Monad m => m ()
day12part2 = return ()

parseInput :: Parsec String u [(String, String)]
parseInput = parseLine `sepEndBy` newline 

parseLine :: Parsec String u (String, String)
parseLine = do 
    word1 <- many1 letter
    char '-' 
    word2 <- many1 letter
    return (word1, word2)


example = "start-A\n\
\start-b\n\
\A-c\n\
\A-b\n\
\b-d\n\
\A-end\n\
\b-end\n"