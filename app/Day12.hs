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
day12part1 = do x <- runParserT parseInput () "example" input12
                print x
                print $ toEdges <$> x
                -- print $ nub <$> next Nothing "start" <$> toEdges <$> x
                print $  length <$> nub <$> next Nothing "start" <$> toEdges <$> x

-- next :: [(String, String)] -> String -> [[String]]
next :: Maybe (String, Int) -> String -> [(String, String)] -> [[String]]
next _ "end"  edges                                          = [["end"]]
next _ [] _                                                 = []
next (Just (s', i)) s edges | isSmallCave s && s' == s && i > 0  = map ([s] ++) $ concatMap (\nextE -> next (Just (s', i + 1)) nextE (moveEdges s edges)) $ nextEdges s edges
next (Just (s', i)) s edges | isSmallCave s && s' == s   = map ([s] ++) $ concatMap (\nextE -> next (Just (s', i + 1)) nextE edges) $ nextEdges s edges
next Nothing s edges        | isSmallCave s             = map ([s] ++) $ concatMap (\nextE -> next (Just (s, 1)) nextE edges ++ next Nothing nextE (moveEdges s edges)) $ nextEdges s edges
next js s edges                                             = map ([s] ++) $ concatMap (\nextE -> next js nextE (moveEdges s edges)) $ nextEdges s edges

isSmallCave :: String -> Bool
isSmallCave "start" = False 
isSmallCave "end" = False 
isSmallCave s = isLower $ head s 

moveEdges :: String -> [(String, String)] -> [(String, String)]
moveEdges s edges = if not $ isSmallCave s then edges else filter (\(a,b) -> a /= s && b /= s) edges

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

appendRev :: [(a,a)] -> [(a,a)]
appendRev a =  a ++ map rev a

fst3 :: (a, b, c) -> a
fst3 (a,b,_) = a
snd3 :: (a, b, c) -> b
snd3 (_,b,_) = b
trd3 :: (a, b, c) -> b
trd3 (_,b,_) = b

rev :: (b, a) -> (a, b)
rev (a,b) = (b,a)

path :: Graph -> VertexInfoMap -> Vertex -> Vertex -> ([Vertex], VertexInfoMap)
path _ m s e | s == e                       = ([], m)
path g m s e | s /= e && elem e (g ! s)     = ([e], m)
path g m s e | s /= e && notElem e (g ! s)  = ([], m)
path a m b c = error $ "something went wrong: " <> show a <> " " <> show b <> " " <> show c

move :: Vertex -> VertexInfoMap -> (Vertex, VertexInfoMap)
move v m = undefined -- update VertexInfoMap state

canMoveTo :: Vertex -> VertexInfoMap -> (Bool, VertexInfoMap)
canMoveTo v m = case Data.Map.lookup v m of
  Nothing -> error $ "invalid state: " <> show v <> ""<> show m
  Just vi -> (canMoveTo' vi, m)

canMoveTo' :: VertexInfo -> Bool
canMoveTo' (Lowercase b) = b
canMoveTo' Uppercase = True

toVertexInfoMap :: Graph -> (String -> Maybe Vertex) -> [String] -> Maybe VertexInfoMap
toVertexInfoMap g l ss = toVertexInfoMap' l (toNodeAndVertexInfo g ss)

toNodeAndVertexInfo :: Graph -> [String] -> [(String, VertexInfo)]
toNodeAndVertexInfo g nodes = zip nodes $ map (\node -> if isUpper (head node) then Uppercase else Lowercase False) nodes

toVertexInfoMap' :: (String -> Maybe Vertex) -> [(String, VertexInfo)] ->Maybe  VertexInfoMap
toVertexInfoMap' l vs =fromList <$> mapM (toVertexInfoMapField l) vs

toVertexInfoMapField :: (String -> Maybe Vertex) -> (String, VertexInfo) -> Maybe (Vertex, VertexInfo)
toVertexInfoMapField l (s, vf) | isNothing (l s) = Nothing
toVertexInfoMapField l (s, vf) = (\a -> (a, vf)) <$> l s

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
