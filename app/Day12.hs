{-# LANGUAGE BlockArguments #-}
module Day12 where
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
import Data.List
import qualified Data.Map as Map
import Control.Monad.State (State, MonadState (get))
import Control.Monad (guard)
import Data.Char (isLower, isUpper)
import qualified Data.Map
-- data Visited = Map Vertex Bool

data Node = Node String deriving (Eq, Show, Ord)

day12part1 :: IO ()
-- day12part1 =  runParserT parseInput Monad.Identity "" example >>= print
day12part1 = do x <- runParserT parseInput () "example" input12
                putStrLn $ show $ toAdjacency <$> x
                let result = (\z -> next (Node "start") [] (toAdjacency z))  <$> x
                -- putStrLn $ show $ sort <$> map (reverse) <$> result
                putStrLn $ show $ length <$> filter ((== (Node "end")) . head) <$> result

next :: Node -> [Node] -> [(Node, [Node]) ] -> [[Node]]
next (Node "end") path edges = [Node "end":path]
next node path edges = concatMap (\a -> next a (node:path) edges) (filter (canGoPart2 (node:path)) (Day12.lookup edges node))

canGoPart1 :: [Node] -> Node -> Bool
canGoPart1 path (Node next) | next == "start" = False
                            | isLower (head next) && elem (Node next) path = False
                            | otherwise = True

canGoPart2 :: [Node] -> Node -> Bool
canGoPart2 path (Node next) | next == "start" = False
                            | isUpper (head next) = True
                            | next == "end" = True
                            | isLower (head next) && notElem (Node next) path = True
                            | isLower (head next) && not (containsAnyLowerCaseTwice (map (\(Node a) -> a) path)) = True
                            | isLower (head next) && containsAnyLowerCaseTwice (map (\(Node a) -> a) path) = False
                            | otherwise = error (show $ "unexpected condition next " ++ show next ++ " path " ++ show path)

containsTwice :: String -> [String] -> Bool
containsTwice str = (>=2) . length . filter (==str)

containsAnyLowerCaseTwice :: [String] -> Bool
containsAnyLowerCaseTwice = any ((>=2) . length) . group .  sort . filter (isLower . head)

lookup :: [(Node, [Node])] -> Node -> [Node]
lookup adj node = snd $ head $ filter ((node==) . fst) adj

toAdjacency :: [(Node, Node)] -> [(Node, [Node]) ]
toAdjacency aaa = map (\a -> (a,sort $ lookupEdge a edges)) nodes
    where
        edges = toEdges aaa
        nodes = nub $ map fst edges

lookupEdge :: Node -> [(Node, Node)] -> [Node]
lookupEdge key = map snd . filter ((==key) . fst)

toEdges :: (Eq a) => [(a,a)] -> [(a,a)]
toEdges lines = lines ++ map reverseT lines

parseInput :: Monad m =>  ParsecT String u m [(Node, Node)]
parseInput =  parseLine `sepEndBy` newline

parseLine :: Monad m => ParsecT String u m (Node, Node)
parseLine = do
    word1 <- many1 letter
    char '-'
    word2 <- many1 letter
    return (Node word1, Node word2)

reverseT :: (a, b) -> (b, a)
reverseT (a,b) = (b,a)

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

