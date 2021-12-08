module Day8 where
import Text.Parsec.Char
import Text.Parsec
import Data.List
import Data.Foldable (Foldable(toList))
import Text.Parsec.String

data Display = Display [String] [String] deriving (Show)

day8p1 :: IO ()
day8p1 = do 
    x <- parseFromFile displays "input8.txt"
    case x of
    -- case parse displays "" example of
      Left pe -> error $ show pe
      Right diss -> print $ length <$> (sequence $ filter (/=Nothing) $ concatMap (\(Display _ right) -> map create1 right) diss)

day8p2 :: IO ()
day8p2 = pure ()

-- data Numbers = One | Seven | Four | Eight

create1 :: String -> Maybe String
create1 n | length n == 2 = Just n
        | length n == 3 = Just n
        | length n == 4 = Just n
        | length n == 7 = Just n
        | otherwise = Nothing

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
