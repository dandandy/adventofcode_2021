module Main where
import Text.Read (readMaybe)
import Text.Show (Show)

data Report = Increased | Decreased deriving (Show, Eq)

main :: IO ()
main = readInput1


readInput1 = do
    x <- readFile "input1.txt"
    print $ countIncreased <$> measure <$> toList x

toList :: String -> Maybe [Int]
toList s = mapM readMaybe ( words s)

measure :: [Int] -> [Report]
measure (x:x':xs) = if x >= x' then Decreased : next else Increased : next
    where next = measure (x':xs)
measure [x] = []
measure [] = []

countIncreased :: [Report] -> Int
countIncreased = foldl (\b a -> if a == Increased then b + 1 else b) 0