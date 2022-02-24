
module Day10 (day10part1, day10part2) where
import Text.Parsec (ParsecT, (<|>), space, parse, endOfLine, spaces, string, eof, choice, many1, sepBy, newline)
import Text.Parsec.Char (char)
import Control.Applicative (Alternative(some))
import Data.List
import Text.Parsec.String (parseFromFile)
import Control.Monad
import Data.Either

import Control.Monad.State
import Control.Monad.Trans.Except
import qualified Control.Monad.Identity as Data.Functor.Identity

brackets = ['{', '}', '(', ')', '[',']', '<', '>']

data Bracket = Curly | Parent | Square | Angle deriving (Show, Eq)

nextState :: BracketState1 -> (BracketOutput, BracketState1)
nextState (Left a) = ([], Left a)
nextState  (Right ([], bb)) = (bb, Right ([], bb) )
nextState (Right (s:ss, [])) | isOpen s = ([charToBracket s], Right (ss, [charToBracket s]))
nextState (Right (s:ss, [])) | isClose s = ([], Left s)
nextState (Right (s:ss, b:bb)) | isOpen s = (newBbs, Right (ss, newBbs))
    where newBbs = charToBracket s : (b:bb)
nextState (Right (s:ss, b:bb)) | isClose s && charToBracket s == b = (bb, Right (ss, bb))
nextState (Right (s:ss, b:bb)) | isClose s && charToBracket s /= b = (b:bb, Left s)

type BracketOutput = [Bracket]
type BracketState1 = Either Char (String, BracketOutput)

checkLetterS :: State BracketState1 BracketOutput
checkLetterS = state nextState

day10part1 :: IO ()
day10part1 = do
    x <- parseFromFile parseInput "input10.txt"
    case x of
      Left pe -> error $  "invalid input " <> show pe
      Right ss -> mainPart1 ss

day10part2 :: IO ()
day10part2 = do
    x <- parseFromFile parseInput "input10.txt"
    -- let x = parse parseInput "example2" example2
    case x of
        Left pe -> error $  "invalid input " <> show pe
        Right ss -> mainPart2 ss


-- main :: String -> Either Char (String, BracketOutput)
-- main :: Foldable t => t a -> BracketState1 -> BracketState1
main :: String -> BracketState1
main ss = execState (replicateM (length ss ) checkLetterS) (initalState ss)

initalState :: String -> BracketState1
initalState ss =  Right (ss, [])

foldFunc :: (String, BracketOutput) -> State BracketState1 BracketOutput -> BracketState1
foldFunc b a = execState a (Right b)

parseInput :: Monad m => ParsecT String u m [[Char]]
parseInput = bracketParser `sepBy` newline

bracketParser :: Monad m => ParsecT String u m [Char]
bracketParser = some $ choice $ map char brackets


mainPart1 :: [String] -> IO ()
mainPart1 = print . sum  . map (toPoints . charToBracket)  . lefts . map main

mainPart2 :: [String] -> IO ()
mainPart2 = print . medianScore . map autocompleteScore . map snd  . rights . map main

medianScore :: [Int] -> Int
medianScore  = median . sort

autocompleteScore :: [Bracket] -> Int
autocompleteScore = foldr ((\a b -> b*5 + a) . toPointsPart2) 0 . reverse

median :: [a] -> a
median a = a !! midpoint
    where midpoint = quot (length a) 2

isOpen :: Char -> Bool
isOpen '(' = True
isOpen '[' = True
isOpen '<' = True
isOpen '{' = True
isOpen _ = False

isClose :: Char -> Bool
isClose = not . isOpen

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