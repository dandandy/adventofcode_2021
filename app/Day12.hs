module Day12  (day12part1, day12part2) where
import qualified Control.Monad as Monad
import Text.Parsec (Parsec, many1, (<|>), sepEndBy, ParsecT, runPT)
import Text.Parsec.Char (letter)
import Text.Parsec (char, newline, parse, runP, runParserT)
import Text.Parsec.Combinator (sepBy)
import qualified Control.Monad.Identity as Monad
import Data.Graph (graphFromEdges)
import Data.Map (Map, empty, toList, insertWith)
import Control.Monad.State (MonadState(state), State, runState, sequence, execState)
import Day8 (SegDisplay(a))

day12part1 :: IO ()
day12part1 = runParserT parseInput Monad.Identity "" example >>= print . ( fst3 <$>  graphFromEdges <$> map toEdge <$> graphFromEdgesState <$>)

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

toEdge :: (String, [String]) -> (String, String, [String])
toEdge (s, a) = (s, s, a)

graphFromEdgesState :: [(String, String)] -> [(String, [String])]
graphFromEdgesState as = toList $ execState (mapM toEdgeMapState as) empty

toEdgeMapState :: (String, String) -> State (Map String [String]) ()
toEdgeMapState sa = state (toEdgeMap sa)

toEdgeMap :: (String, String) -> Map String [String] -> ((), Map String [String])
toEdgeMap (s, a) m = ((), insertWith (<>) s [a] m)

prependString :: String -> [String] -> Maybe [String]
prependString a = Just . (a:)


fst3 :: (a, b, c) -> a
fst3 (a,b,_) = a
example = "start-A\n\
\start-b\n\
\A-c\n\
\A-b\n\
\b-d\n\
\A-end\n\
\b-end\n"