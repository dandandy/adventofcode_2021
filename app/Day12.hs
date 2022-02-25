module Day12  (day12part1, day12part2) where
import qualified Control.Monad as Monad
import Text.Parsec (Parsec, many1, (<|>), sepEndBy, ParsecT, runPT)
import Text.Parsec.Char (letter)
import Text.Parsec (char, newline, parse, runP, runParserT)
import Text.Parsec.Combinator (sepBy)
import qualified Control.Monad.Identity as Monad
import Data.Graph (graphFromEdges)

day12part1 :: IO ()
day12part1 = (\a -> (\(a,_,_) -> a) <$> graphFromEdges  <$>  map toEdge <$> a) <$> runParserT parseInput Monad.Identity "" example >>= print

day12part2 :: Monad.Monad m => m ()
day12part2 = return ()

parseInput :: ParsecT String u IO [(String, String)]
parseInput = parseLine `sepEndBy` newline 

parseLine :: ParsecT String u IO (String, String)
parseLine = do 
    word1 <- many1 letter
    char '-' 
    word2 <- many1 letter
    return (word1, word2)

toEdge :: (String, String) -> (String, String, [String])
toEdge (s, a) = (s, s, [a])



example = "start-A\n\
\start-b\n\
\A-c\n\
\A-b\n\
\b-d\n\
\A-end\n\
\b-end\n"