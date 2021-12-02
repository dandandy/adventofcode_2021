module Main where
import Text.Read (readMaybe)
import Text.Show (Show)

data Report = Increased | Decreased | NoChange deriving (Show, Eq)

main :: IO ()
main = readInput2


example :: Maybe [Int]
example = toList "199 200 208 210 200 207 240 269 260 263"

readInputFile :: IO (Maybe [Int])
readInputFile = do
    x <- readFile "input1.txt"
    return $ toList x

readInput1 = do
    x <- readInputFile
    print $ countIncreased . measure <$> x

readInput2 = do
    x <- readInputFile
    print $ countIncreased . measure . rollingWindow <$> x

toList :: String -> Maybe [Int]
toList s = mapM readMaybe ( words s)

measure :: [Int] -> [Report]
measure (x:x':xs) = comparison x x' : measure (x':xs)
    where comparison x y 
            | x == y = NoChange
            | x < y = Increased
            | x > y = Decreased
            | otherwise = NoChange
measure [x] = []
measure [] = []

countIncreased :: [Report] -> Int
countIncreased = foldl (\b a -> if a == Increased then b + 1 else b) 0

rollingWindow :: [Int] -> [Int]
rollingWindow (x:x':x'':xs) = x + x' + x'' : rollingWindow (x':x'':xs)
rollingWindow _ = []