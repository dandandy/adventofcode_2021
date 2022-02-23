
module Day10 (day10part1, day10part2) where
import Text.Parsec (ParsecT, (<|>), space, parse, endOfLine, spaces, string, eof, choice, many1, sepBy, newline)
import Text.Parsec.Char (char)
import Control.Applicative (Alternative(some))
import Data.List
import Text.Parsec.String (parseFromFile)
import Control.Monad
import Data.Either

brackets = ['{', '}', '(', ')', '[',']', '<', '>']

data Bracket = Curly | Parent | Square | Angle deriving (Show)

day10part1 :: IO ()
day10part1 = do
    x <- parseFromFile parseInput "input10.txt"
    case x of
      Left pe -> error $  "invalid input " <> show pe
      Right ss -> mainPart1 ss

day10part2 :: IO ()
day10part2 = do
    x <- parseFromFile parseInput "input10.txt"
    -- x <- pure $ parse parseInput "example2" example2
    case x of
        Left pe -> error $  "invalid input " <> show pe
        Right ss -> print $ medianScore $ map autocompleteScore $ findValidIncompleteLines ss

parseInput :: Monad m => ParsecT String u m [[Char]]
parseInput = bracketParser `sepBy` newline

bracketParser :: Monad m => ParsecT String u m [Char]
bracketParser = some $ choice $ map char brackets

mainPart1 :: [String] -> IO ()
mainPart1 ss = print $ sum $ map toPoints $ map charToBracket $ lefts $ map (`run` []) ss

mainPart2 :: [String] -> IO ()
mainPart2 = print . medianScore . map autocompleteScore . findValidIncompleteLines

medianScore :: [Int] -> Int 
medianScore  = median . sort 

findValidIncompleteLines :: [String] -> [[Bracket]]
findValidIncompleteLines = rights . map (`run` [])

autocompleteScore :: [Bracket] -> Int 
autocompleteScore = foldr ((\a b -> b*5 + a) . toPointsPart2) 0 . reverse

run :: String -> [Char] -> Either Char [Bracket]
(s:ss) `run` stack = run ss =<< takeFromStringAndPutOnStack (s:ss) stack
[] `run` stack = Right $ map charToBracket stack

takeFromStringAndPutOnStack :: String -> [Char] -> Either Char [Char]
takeFromStringAndPutOnStack (s:ss) [] = if s == '(' || s == '{' || s == '<' || s == '[' then Right [s] else Left s
takeFromStringAndPutOnStack (s:ss) (st:sts) = if compareInputToStack s st then Right (if s == ')' || s == '}' || s == ']' || s == '>' then sts else s:st:sts ) else Left s
takeFromStringAndPutOnStack [] a = Right a

median :: [a] -> a
median a = a !! midpoint
    where midpoint = quot (length a) 2

-- Top of String Input -> Top of Stack 
compareInputToStack :: Char -> Char -> Bool
compareInputToStack '(' _ = True
compareInputToStack '[' _ = True
compareInputToStack '{' _ = True
compareInputToStack '<' _ = True
compareInputToStack ')' '(' = True
compareInputToStack ']' '[' = True
compareInputToStack '}' '{' = True
compareInputToStack '>' '<' = True
compareInputToStack a _ = False


push :: [a] -> a -> [a]
push xs x = x:xs

pop :: [a] -> ([a], Maybe a)
pop (x:xs) = (xs, Just x)
pop [] = ([], Nothing )

isValid :: Char -> Char -> Bool
isValid '(' ')' = True
isValid '[' ']' = True
isValid '<' '>' = True
isValid '{' '}' = True
isValid _ _ = False

toPoints :: Bracket -> Int
toPoints Parent = 3
toPoints Square = 57
toPoints Curly = 1197
toPoints Angle = 25137

toPointsPart2 :: Bracket -> Int
toPointsPart2 Parent = 1
toPointsPart2 Square = 2
toPointsPart2 Curly = 3
toPointsPart2 Angle = 4

charToBracket :: Char -> Bracket
charToBracket '<' = Angle
charToBracket '>' = Angle
charToBracket '{' = Curly
charToBracket '}' = Curly
charToBracket '[' = Square
charToBracket ']' = Square
charToBracket ')' = Parent
charToBracket '(' = Parent
charToBracket a = error $ "invalid char to bracket " <> show a

-- example = "<{([([[(<>()){}]>(<<{{"
example = "[({(<(())[]>[[{[]{<()<>>\n\
\[(()[<>])]({[<{<<[]>>(\n\
\{([(<{}[<>[]}>{[]{[(<()>\n\
\(((({<>}<{<{<>}{[]{[]{}\n\
\[[<[([]))<([[{}[[()]]]\n\
\[{[{({}]{}}([{[{{{}}([]\n\
\{<[[]]>}<{[{[{[]{()[[[]\n\
\[<(<(<(<{}))><([]([]()\n\
\<{([([[(<>()){}]>(<<{{\n\
\<{([{{}}[<[[[<>{}]]]>[]]"


example2 = "<{([{{}}[<[[[<>{}]]]>[]]"