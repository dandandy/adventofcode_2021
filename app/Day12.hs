{-# LANGUAGE BlockArguments #-}
module Day12  (day12part1, day12part2) where
import qualified Control.Monad as Monad
import Text.Parsec
    ( Parsec,
      many1,
      (<|>),
      sepEndBy,
      ParsecT,
      runPT,
      char,
      newline,
      parse,
      runP,
      runParserT )
import Text.Parsec.Char (letter)
import Text.Parsec.Combinator (sepBy)
import qualified Control.Monad.Identity as Monad
import Data.Graph (graphFromEdges, Graph, Vertex, reachable)
import Text.Parsec.Error (ParseError)
import GHC.IO
import Control.Monad.Trans.Except (ExceptT)
import Data.List (nub)
import qualified Data.Map as Map
import Control.Monad.State (State, MonadState (get))
import Control.Monad (guard)
import Data.Char (isLower, isUpper)


-- data Visited = Map Vertex Bool

day12part1 :: IO ()
-- day12part1 =  runParserT parseInput Monad.Identity "" example >>= print
day12part1 = do x <- runParserT parseInput () "input12" input12
                print x
                print $ toEdges <$> x
                print $  flip next "start" <$> toEdges <$> x
                print $  length <$> flip next "start" <$> toEdges <$> x

-- next :: [(String, String)] -> String -> [[String]]
next :: [(String, String)] -> String -> [[String]]
next edges "end"  = [["end"]]
next [] _ = []
next edges s = map ([s] ++) $ concatMap (next (moveEdges s edges)) $ nextEdges s edges

moveEdges :: String -> [(String, String)] -> [(String, String)]
moveEdges s edges = if isUpper (head s) then edges else filter (\(a,b) -> a /= s && b /= s) edges

nextEdges :: String -> [(String, String)] -> [String]
-- nextEdges "end" _ = []
nextEdges s a  = map snd $ filter (\(a,b) -> a == s) a

toEdges :: [(String, String)] -> [(String, String)]
toEdges as = filter (\(a,b) -> b /= "start" && a /= "end") $ as ++ map reverseT as

day12part2 :: Monad.Monad m => m ()
day12part2 = return ()

parseInput :: Monad m =>  ParsecT String u m [(String, String)]
parseInput = parseLine `sepEndBy` newline

parseLine :: Monad m => ParsecT String u m (String, String)
parseLine = do
    word1 <- many1 letter
    char '-'
    word2 <- many1 letter
    return (word1, word2)

toEdge :: (String, String) -> (String, String, [String])
toEdge (s, a) = (s, s, [a])

reverseT :: (a, b) -> (b, a)
reverseT (a,b) = (b,a)

matching :: Eq a => a -> [(a,b)] -> [b]
matching a = map snd . filter (\(x,y) -> x == a)

example = "start-A\n\
\start-b\n\
\A-c\n\
\A-b\n\
\b-d\n\
\A-end\n\
\b-end\n"

example2 = "dc-end\n\
\HN-start\n\
\start-kj\n\
\dc-start\n\
\dc-HN\n\
\LN-dc\n\
\HN-end\n\
\kj-sa\n\
\kj-HN\n\
\kj-dc"


input12="TR-start\n\
\xx-JT\n\
\xx-TR\n\
\hc-dd\n\
\ab-JT\n\
\hc-end\n\
\dd-JT\n\
\ab-dd\n\
\TR-ab\n\
\vh-xx\n\
\hc-JT\n\
\TR-vh\n\
\xx-start\n\
\hc-ME\n\
\vh-dd\n\
\JT-bm\n\
\end-ab\n\
\dd-xx\n\
\end-TR\n\
\hc-TR\n\
\start-vh"
