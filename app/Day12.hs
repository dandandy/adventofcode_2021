module Day12  (day12part1, day12part2) where
import qualified Control.Monad as Monad
import Text.Parsec (Parsec, many1, (<|>), sepEndBy, ParsecT, runPT)
import Text.Parsec.Char (letter)
import Text.Parsec (char, newline, parse, runP, runParserT)
import Text.Parsec.Combinator (sepBy)
import qualified Control.Monad.Identity as Monad
import Data.Graph (graphFromEdges, Graph, Vertex)
import Data.Map (Map, empty, toList, insertWith, fromList)
import Control.Monad.State (MonadState(state), State, runState, sequence, execState, liftM)
import GHC.Arr (bounds, (!))
import Data.Char
import Data.Maybe

data VertexInfo = Lowercase Bool | Uppercase deriving (Show)
type VertexInfoMap = Map Vertex VertexInfo

day12part1 :: IO ()
-- day12part1 = runParserT parseInput Monad.Identity "" example >>= print . ( fst3 <$>  graphFromEdges <$> map toEdge <$> graphFromEdgesState <$>)
day12part1 = do Right x <- runParserT parseInput Monad.Identity "" example
                let edges = (graphFromEdgesState . appendRev) x
                print edges
                let (a,b,c) = (graphFromEdges .  map toEdge) edges
                print a
                print $ bounds a
                print $ map b [fst $ bounds a.. snd $ bounds a]
                print $ map c ["start"]
                let vim = toVertexInfoMap a c (map fst edges)
                print $ path a <$> vim <*> c "start" <*> c "end"

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


toVertexInfoMap :: Graph -> (String -> Maybe Vertex) -> [String] -> Maybe VertexInfoMap
toVertexInfoMap g l ss = toVertexInfoMap' l (toNodeAndVertexInfo g ss)

toNodeAndVertexInfo :: Graph -> [String] -> [(String, VertexInfo)]
toNodeAndVertexInfo g nodes = zip nodes $ map (\node -> if isUpper (head node) then Uppercase else Lowercase False) nodes

toVertexInfoMap' :: (String -> Maybe Vertex) -> [(String, VertexInfo)] ->Maybe  VertexInfoMap
toVertexInfoMap' l vs =fromList <$> mapM (toVertexInfoMapField l) vs

toVertexInfoMapField :: (String -> Maybe Vertex) -> (String, VertexInfo) -> Maybe (Vertex, VertexInfo)
toVertexInfoMapField l (s, vf) | isNothing (l s) = Nothing
toVertexInfoMapField l (s, vf) = (\a -> (a, vf)) <$> l s



example = "start-A\n\
\start-b\n\
\A-c\n\
\A-b\n\
\b-d\n\
\A-end\n\
\b-end\n"