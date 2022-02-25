module Day11 (day11part1) where
import Text.Parsec
import qualified Data.Matrix as M
import Day9 (neighboursIndex, safeGetWithIndex)
import Text.Parsec.String (parseFromFile)
import qualified Data.Maybe as Maybe
import qualified Data.Matrix as M.Matrix
import Control.Monad.State (StateT, state, MonadState (get, put), runState)
import qualified Control.Monad.Identity as Monad
import qualified Data.Functor.Classes as M

type Point = (Int, Int)

-- day11part1Parser :: Monad m => ParsecT String u m [[Int]]
day11part1Parser :: Monad m => ParsecT String u m (M.Matrix Int)
day11part1Parser = M.fromLists <$> parseInput

parseInput :: Monad m => ParsecT String u m [[Int]]
parseInput = manyDigit `sepBy` newline

manyDigit :: Monad m =>  ParsecT String u m [Int]
manyDigit = many1 (read . pure <$> digit)

day11part1 :: IO ()
day11part1 = do
    -- x <- parseFromFile day11part1Parser "input10.txt"
    let x = parse day11part1Parser "input10.txt" example2
    print x
    let x' = snd <$> runState (sequence ([incrementAllM >> return Nothing]<> concat (replicate 6 [isComplete >>= return . Just, stepM' >> return Nothing]))) <$> x
    
    let y = parse day11part1Parser "input10.txt" example3
    print $ (==) <$> x' <*> y

type OctoState a = StateT (M.Matrix Int) Monad.Identity a

-- main :: OctoState

main :: StateT (M.Matrix.Matrix Int) Monad.Identity [Int]
main = Monad.replicateM 1 startStep


startStep = do
    m <- get
    incrementAllM
    calculateStep

calculateStep :: OctoState Int
calculateStep = do  b <- isComplete
                    if b
                        then return 0
                        else calculateStep

stepM :: OctoState Int
stepM = state step

step :: M.Matrix Int -> (Int, M.Matrix Int)
step m = (length flashIndexes, setFlashedIndexToZero)
    where
          flashIndexes = getFlashesIndex m
          flashNeighbours = filter (`isZeroValue` m) $  concatMap (`getNeighbours` m) flashIndexes
          afterFlashes = foldl (flip incrementAt) m flashNeighbours
          setFlashedIndexToZero = foldl (flip setToZero) afterFlashes flashIndexes

isComplete :: OctoState Bool
isComplete = do null . getFlashesIndex <$> get

stepM' :: OctoState Int
stepM' = do m <- get
            let flashIndexes = getFlashesIndex m
            let flashNeighbours =  concatMap (`getNeighbours` m) flashIndexes
            mapM_ incrementAtIfNotZero flashNeighbours
            mapM_ setToZeroM flashIndexes
            return $ length flashIndexes

incrementAllM :: OctoState ()
incrementAllM = get >>= put . incrementAll

incrementAll :: (Num b, Traversable t) => t b -> t b
incrementAll= fmap (+1)

getNeighbours :: Point -> M.Matrix Int -> [Point]
getNeighbours = allValidAdjacentNeighbours

isZeroValue :: Point -> M.Matrix Int -> Bool
isZeroValue (a,b) = (==0) . M.getElem a b

incrementAtIfNotZero :: Point -> OctoState Bool
incrementAtIfNotZero (x,y) = get >>= (\m -> if M.getElem x y m == 0 then return False else incrementAtM (x,y) >> return True)

incrementAtM :: Point -> OctoState ()
incrementAtM p = put . incrementAt p =<< get

incrementAt :: Point -> M.Matrix Int -> M.Matrix Int
incrementAt = updateWith (+ 1)

setToZeroM :: Point -> OctoState ()
setToZeroM p = put . setToZero p =<< get

setToZero :: Point -> M.Matrix Int -> M.Matrix Int
setToZero = updateWith (const 0)

flashes :: Int -> Bool
flashes a = a > 9

updateWith :: (Int -> Int) -> Point -> M.Matrix Int -> M.Matrix Int
updateWith f (x, y) m = M.setElem (f oldValue) (x,y) m
    where oldValue = M.getElem x y m


allValidAdjacentNeighbours :: Point -> M.Matrix a -> [Point]
allValidAdjacentNeighbours p m =  maybe [] (map (\ (a,b,c) -> (a,b)))  $  sequence $ filter Maybe.isJust $ allAdjacentNeighbours p m

allAdjacentNeighbours :: Point -> M.Matrix a -> [Maybe (Int, Int, a)]
allAdjacentNeighbours (x,y) m =  neighboursIndex x y m <> [safeGetWithIndex (x - 1) (y + 1) m, safeGetWithIndex (x - 1) (y - 1) m, safeGetWithIndex (x + 1) (y + 1) m, safeGetWithIndex (x + 1) (y - 1) m]

getFlashesIndex :: M.Matrix Int -> [Point]
getFlashesIndex m = [(x, y) |  x <- [1..10], y <- [1..10], flashes (M.getElem x y m) ]


isEqual :: (Eq a) => M.Matrix a -> M.Matrix a -> Bool
isEqual = (==)

example1 = "5483143223\n\
\2745854711\n\
\5264556173\n\
\6141336146\n\
\6357385478\n\
\4167524645\n\
\2176841721\n\
\6882881134\n\
\4846848554\n\
\5283751526"


example2 = "6594254334\n\
\3856965822\n\
\6375667284\n\
\7252447257\n\
\7468496589\n\
\5278635756\n\
\3287952832\n\
\7993992245\n\
\5957959665\n\
\6394862637"

example3 = "8807476555\n\
\5089087054\n\
\8597889608\n\
\8485769600\n\
\8700908800\n\
\6600088989\n\
\6800005943\n\
\0000007456\n\
\9000000876\n\
\8700006848"