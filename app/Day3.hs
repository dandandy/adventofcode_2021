module Day3 (day3part1, day3part2) where
import Data.List
import Debug.Trace

getFile = readFile "input3.txt"

example :: IO String
example = do return "00100 11110 10110 10111 10101 01111 00111 11100 10000 11001 00010 01010"
day3part1 = do  x <- getFile
                putStrLn $ (("Day 3 Part 1: "++) . show . multiplyTuple . applyToTuple binaryToInt .  applyFunction notBinary . map mostCommon . transpose . parseInput) x

day3part2 = do  x <- example
                putStrLn $ (("Day 3 Part 2: "++) . show .oxygenGeneratorRating . parseInput) x

parseInput :: String -> [[Int]]
parseInput i = map (map readChar) $ words i

readChar :: Char -> Int
readChar = read . pure

oxygenGeneratorRating :: [[Int]] -> [Int]
oxygenGeneratorRating = oxygenBitCriteria 0

oxygenBitCriteria :: Int -> [[Int]] -> [Int]
oxygenBitCriteria _ [v] = v
oxygenBitCriteria n vs
    | null vs || n > length (head vs) - 1 = error $ show vs ++ " error!! " ++ show n
    | length vs > 1 = trace (show n ++ " " ++ show vs ++ " " ++ show (filter (\xs -> (xs !! n) == common') vs) ++ " " ++ show common') oxygenBitCriteria (n+1) $ filter (\xs -> (xs !! n) == common') vs
    where common' = common n vs
oxygenBitCriteria _ _ = []

common :: Int -> [[Int]] -> Int
common n =  mostCommon . map (!! n)

mostCommon :: [Int] -> Int
mostCommon l = case  partition (==1) l of
    (x, y) -> if length x >= length y then x !! 1 else y !! 1

binaryToInt :: [Int] -> Int
binaryToInt xs = sum ( zipWith (\ x y -> x * 2 ^ y) (reverse xs) (iterate (+1) 0))

notBinary :: [Int] -> [Int]
notBinary = map (\x -> if x == 0 then 1 else 0)

applyFunction :: (a -> b) -> a  -> (a, b)
applyFunction f a = (a, f a)

applyToTuple :: (a -> b) -> (a,a) ->  (b, b)
applyToTuple f (a,b) = (f a, f b)

multiplyTuple :: Num a => (a, a) -> a
multiplyTuple (a,b) = a * b
