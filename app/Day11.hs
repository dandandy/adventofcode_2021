module Day11 (day11part1Parser, day11part1Application) where
import Text.Parsec
import qualified Data.Matrix as M
import Day9 (neighboursIndex)
import qualified Data.IntMap as M

day11part1Parser :: Monad m => ParsecT String u m [[Int]]
day11part1Parser = parseInput


manyDigit :: Monad m =>  ParsecT String u m [Int]
manyDigit = many1 (read . pure <$> digit)

parseInput :: Monad m => ParsecT String u m [[Int]]
parseInput = manyDigit `sepBy` newline

day11part1Application :: M.Matrix Int -> IO ()
day11part1Application = undefined


incrementAll :: Num b => M.IntMap b -> M.IntMap b
incrementAll= M.map (+1)


-- incrementNinesFlashes :: Num b => M.IntMap b -> M.IntMap b
-- incrementNinesFlashes m = M.