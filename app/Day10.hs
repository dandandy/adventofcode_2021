
module Day10 (day10part1, day10part2) where
import Text.Parsec (ParsecT, (<|>), space, parse, endOfLine, spaces, string, eof, choice, many1, sepBy, newline)
import Text.Parsec.Char (char)
import Control.Applicative (Alternative(some))
import Data.List
import Text.Parsec.String (parseFromFile)
import Control.Monad

brackets = ['{', '}', '(', ')', '[',']', '<', '>']

day10part1 :: IO ()
day10part1 = do
    x <- parseFromFile parseInput "input10.txt"
    case x of
      Left pe -> error $  "invalid input " <> show pe
      Right ss -> mainPart1 ss

day10part2 :: IO ()
day10part2 = pure ()

parseInput :: Monad m => ParsecT String u m [[Char]]
parseInput = bracketParser `sepBy` newline

bracketParser :: Monad m => ParsecT String u m [Char]
bracketParser = some $ choice $ map char brackets

mainPart1 :: [String] -> IO ()
mainPart1 ss = print $ sum $ map (leftToPoint . (`run` [])) ss

mainPart2 :: [String] -> IO ()
mainPart2 ss = print $ map (`run` []) ss

run :: String -> [Char] -> Either Char [Char]
(s:ss) `run` stack = run ss =<< takeFromStringAndPutOnStack (s:ss) stack
[] `run` stack = Right stack

takeFromStringAndPutOnStack :: String -> [Char] -> Either Char [Char]
takeFromStringAndPutOnStack (s:ss) [] = if s == '(' || s == '{' || s == '<' || s == '[' then Right [s] else Left s
takeFromStringAndPutOnStack (s:ss) (st:sts) = if compareInputToStack s st then Right (if s == ')' || s == '}' || s == ']' || s == '>' then sts else s:st:sts ) else Left s
takeFromStringAndPutOnStack [] a = Right a


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

toPoints :: Char -> Int
toPoints ')' = 3
toPoints ']' = 57
toPoints '}' = 1197
toPoints '>' = 25137
toPoints _ = 0

leftToPoint :: Either Char a -> Int
leftToPoint (Left a) = toPoints a
leftToPoint _ = 0

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