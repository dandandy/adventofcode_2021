module Day5 (day5part1, day5part2) where
import Text.Parsec hiding (Line)
import Text.Parsec ()
import Data.List
import Text.Parsec.String (parseFromFile)

data Point = Point Int Int deriving (Show, Eq)
data Line = Line Point Point deriving (Show)

day5part1 :: IO ()
day5part1 = do  x <- parseFromFile inputlines "input5.txt"
                print (length . filter (>=2) . countCopies . concatMap getLinePoints . filter isStraightOrHoriz <$> x) --  

day5part2 :: IO ()
day5part2 = pure ()

number :: Monad m => ParsecT String u m Int
number = read <$> many1 digit

coord :: Monad m => ParsecT String u m Point
coord = Point <$> (number <* char ',') <*> number

getLinePoints :: Line -> [Point]
getLinePoints l     | isStraight l = case l of
    Line (Point x1 y1) (Point x2 y2)    | y1 == y2 -> [Point x1 y1]
                                        | y1 < y2 -> Point x1 y1 : getLinePoints (Line (Point x1 (y1 + 1)) (Point x2 y2))
                                        | y1 > y2 -> Point x1 y1 : getLinePoints (Line (Point x1 (y1 - 1)) (Point x2 y2))
    Line _ _ -> []
getLinePoints l     | isHoriz l = case l of
    Line (Point x1 y1) (Point x2 y2)    | x1 == x2 -> [Point x1 y1]
                                        | x1 < x2 -> Point x1 y1 : getLinePoints (Line (Point (x1 + 1) y1) (Point x2 y2))
                                        | x1 > x2 -> Point x1 y1 : getLinePoints (Line (Point (x1 - 1) y1) (Point x2 y2))
    Line _ _ -> []
getLinePoints l   = []

inputline :: Monad m => ParsecT String u m Line
inputline = Line <$> coord <* char ' ' <*> (arrow *> char ' ' *> coord)

inputlines ::  Monad m => ParsecT String u m [Line]
inputlines = inputline `sepBy` newline

arrow :: Monad m => ParsecT String u m ()
arrow =  sequence_ [char '-',char '>']

isStraightOrHoriz :: Line -> Bool
isStraightOrHoriz l = isStraight l || isHoriz l

isHoriz :: Line -> Bool
isHoriz (Line (Point _ y1) (Point _ y2)) = y1 == y2

isStraight :: Line -> Bool
isStraight (Line (Point x1 _) (Point x2 _)) = x1 == x2

countCopies :: Eq a => [a] -> [Int]
countCopies xs = map (\a -> length (filter (==a) xs))  uniq
    where uniq = nub xs

example :: IO String
example = pure "0,9 -> 5,9\n\
\8,0 -> 0,8\n\
\9,4 -> 3,4\n\
\2,2 -> 2,1\n\
\7,0 -> 7,4\n\
\6,4 -> 2,0\n\
\0,9 -> 2,9\n\
\3,4 -> 1,4\n\
\0,0 -> 8,8\n\
\5,5 -> 8,2"