module Day8 where
import Text.Parsec.Char
import Text.Parsec
import Data.List
import Data.Foldable (Foldable(toList))
import Text.Parsec.String
import qualified Data.Maybe

data Display = Display [String] [String] deriving (Show)

data SegDisplay = SD { a::Char, b::Char, c:: Char,d:: Char,e:: Char, f:: Char, g:: Char }

day8p1 :: IO ()
day8p1 = do
    x <- parseFromFile displays "input8.txt"
    case x of
    -- case parse displays "" example of
      Left pe -> error $ show pe
      Right diss -> print $ length <$> (sequence $ filter (/=Nothing) $ concatMap (\(Display _ right) -> map create right) diss)

day8p2 :: IO ()
day8p2 = pure ()

-- data Numbers = One | Seven | Four | Eight

isUnique :: String -> Bool
isUnique n = length n `elem` [2,3,4,7]

find1 xs = Data.Maybe.fromMaybe [] (find (\ s -> length s == 2) xs)

find4 xs = Data.Maybe.fromMaybe [] (find (\ s -> length s == 4) xs)

find7 :: Foldable t => t [a] -> [a]
find7 xs = Data.Maybe.fromMaybe [] (find (\ s -> length s == 3) xs)

find8 xs = Data.Maybe.fromMaybe [] (find (\ s -> length s == 7) xs)

find3 :: [String] -> Maybe String
find3 xs = find (\s -> seven == seven `intersect` s) $ filter (\s -> length s == 5) xs
    where seven = find7 xs

find5 :: [String] -> Maybe String
find5 xs = find (\s -> threeSeven == threeSeven `intersect` s) $ filter (\s -> length s == 5) xs
    where   threeSeven = nub (three ++ seven) \\ one
            three = Data.Maybe.fromMaybe [] $ find3 xs
            seven = find7 xs
            one = Data.Maybe.fromMaybe [] (find (\ s -> length s == 2) xs)

find2 :: [String] -> Maybe String
find2 xs = find (\s -> s /= five && s /= three && length s == 5) xs
    where   five = Data.Maybe.fromMaybe [] $ find5 xs
            three = Data.Maybe.fromMaybe [] $ find3 xs

find6 xs = find (\s -> five `intersect` s == five) $ filter (\s -> length s == 6) xs
    where   five = nub $ Data.Maybe.fromMaybe [] (find5 xs) ++ g
            one = find1 xs
            g = two \\ three
            two = Data.Maybe.fromMaybe [] $ find2 xs
            three = Data.Maybe.fromMaybe [] $ find3 xs


find9 :: [[Char]] -> Maybe [Char]
find9 xs = find (\s -> length s == 6 && s /= six && five `intersect` s == five) xs
    where   six = Data.Maybe.fromMaybe [] $ find6 xs
            five = nub $ Data.Maybe.fromMaybe [] (find5 xs) 

find0 xs = find (\s -> s /= nine && s /= six && length s == 6) xs
    where   nine = nub $ Data.Maybe.fromMaybe [] (find9 xs) 
            six = nub $ Data.Maybe.fromMaybe [] (find6 xs) 

create :: String -> Maybe Int
create n | length n == 2 = Just 1
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
countCopies xs = map (\a -> (a, length (filter (==a) xs)))  uniq
    where uniq = nub xs

example = "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe\n\
\edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc\n\
\fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg\n\
\fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb\n\
\aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea\n\
\fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb\n\
\dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe\n\
\bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef\n\
\egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb\n\
\gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
