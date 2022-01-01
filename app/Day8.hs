module Day8 where

import Control.Monad (join)
import Data.List (find, nub)
import Data.Map (Map, fromList, toList)
import qualified Data.Map
import Data.Maybe
import qualified Data.Maybe
import Data.Set (Set, empty, intersection, isSubsetOf, size, (\\))
import qualified Data.Set (fromList)
import Debug.Trace (trace)
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

data Display = Display [String] [String] deriving (Show)

data SegDisplay = SD {a :: Char, b :: Char, c :: Char, d :: Char, e :: Char, f :: Char, g :: Char}

day8p1 :: IO ()
day8p1 = do
  x <- parseFromFile displays "input8.txt"
  case x of
    -- case parse displays "" example of
    Left pe -> error $ show pe
    Right diss -> print $ length <$> (sequence $ filter (/= Nothing) $ concatMap (\(Display _ right) -> map create right) diss)

day8p2 :: IO ()
day8p2 = do
  x <- parseFromFile displays "input8.txt"
  case x of
--   case parse displays "" line14 of
    Left pe -> error $ show pe
    Right diss -> print $ sum <$> map read <$> mapM findDisplayValue diss
    -- Right diss -> print $ map fst $ filter (\(a,b) -> Data.Maybe.isNothing b) $ zip (iterate (+1) 1)  (map findDisplayValue diss)
    -- Right diss -> print $ map findAssignments diss

toTuple :: a -> b -> (b, a)
toTuple x s = (s, x)

-- findAssignments :: Display -> [Maybe (String, Int)]
findAssignments :: Display -> Maybe (Map (Set Char) Int)
findAssignments (Display left right) = run left

findDisplayValue :: Display -> Maybe String
findDisplayValue (Display left right) = concatMap show <$> ((`lookupInMap` rightSets) =<< run left)
  where
    rightSets = map Data.Set.fromList right

lookupInMap :: Ord a => Map a b -> [a] -> Maybe [b]
lookupInMap m = mapM (`Data.Map.lookup` m)

run :: [String] -> Maybe (Map (Set Char) Int)
run s = fromList <$> sequence (rules <*> [map Data.Set.fromList s])

rules :: [[Set Char] -> Maybe (Set Char, Int)]
rules = [fmap (toTuple 0) . find0, fmap (toTuple 1) . find1, fmap (toTuple 2) . find2, fmap (toTuple 3) . find3, fmap (toTuple 4) . find4, fmap (toTuple 5) . find5, fmap (toTuple 6) . find6, fmap (toTuple 7) . find7, fmap (toTuple 8) . find8, fmap (toTuple 9) . find9]

isUnique :: String -> Bool
isUnique n = length n `elem` [2, 3, 4, 7]

find1 :: [Set Char] -> Maybe (Set Char)
find1 = find (\s -> size s == 2)

find4 :: [Set Char] -> Maybe (Set Char)
find4 = find (\s -> size s == 4)

find7 :: [Set Char] -> Maybe (Set Char)
find7 = find (\s -> size s == 3)

find8 :: [Set Char] -> Maybe (Set Char)
find8 = find (\s -> size s == 7)

find3 :: [Set Char] -> Maybe (Set Char)
find3 xs = find (\s -> seven `isSubsetOf` s && length s == 5) xs
  where
    seven = fromMaybe Data.Set.empty (find7 xs)

find5 :: [Set Char] -> Maybe (Set Char)
find5 xs = find (\s -> three /= s && length s == 5 && fourMinusOne `isSubsetOf` s) xs
  where
    fourMinusOne = four \\ one
    three = Data.Maybe.fromMaybe Data.Set.empty $ find3 xs
    one = Data.Maybe.fromMaybe Data.Set.empty $ find1 xs
    four = Data.Maybe.fromMaybe Data.Set.empty $ find4 xs

find2 :: [Set Char] -> Maybe (Set Char)
find2 xs = find (\s -> s /= five && s /= three && length s == 5) xs
  where
    five = Data.Maybe.fromMaybe Data.Set.empty $ find5 xs
    three = Data.Maybe.fromMaybe Data.Set.empty $ find3 xs

find6 :: [Set Char] -> Maybe (Set Char)
find6 xs = find (\s -> length s == 6 && s /= zero && s /= nine) xs
  where
    zero = Data.Maybe.fromMaybe Data.Set.empty $ find0 xs
    nine = Data.Maybe.fromMaybe Data.Set.empty $ find9 xs

find9 :: [Set Char] -> Maybe (Set Char)
find9 xs = find (\s -> length s == 6 && four `isSubsetOf` s) xs
  where
    four = Data.Maybe.fromMaybe Data.Set.empty $ find4 xs

find0 :: [Set Char] -> Maybe (Set Char)
find0 xs = find (\s -> length s == 6 && seven `isSubsetOf` s && not (four `isSubsetOf` s)) xs
  where
    seven = Data.Maybe.fromMaybe Data.Set.empty (find7 xs)
    four = Data.Maybe.fromMaybe Data.Set.empty (find4 xs)

create :: String -> Maybe Int
create n
  | length n == 2 = Just 1
  | length n == 3 = Just 7
  | length n == 4 = Just 4
  | length n == 7 = Just 8
  | otherwise = Nothing

validWord :: Monad m => ParsecT String u m String
validWord = many1 $ oneOf "abcdefg"

part :: Monad m => ParsecT String u m [String]
part = validWord `sepEndBy` char ' '

display :: Monad m => ParsecT String u m Display
display = Display <$> part <*> (char '|' *> char ' ' *> part)

displays :: Monad m => ParsecT String u m [Display]
displays = display `sepBy` newline

countCopies :: Eq a => [a] -> [(a, Int)]
countCopies xs = map (\a -> (a, length (filter (== a) xs))) uniq
  where
    uniq = nub xs

example =
  "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe\n\
  \edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc\n\
  \fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg\n\
  \fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb\n\
  \aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea\n\
  \fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb\n\
  \dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe\n\
  \bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef\n\
  \egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb\n\
  \gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"

line14 = "dcagfeb daf decgf fa dcfag gdacfe dbgcef cbdga aedcbf fega | gafcd fad afd fdaceb"