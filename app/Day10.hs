
module Day10 (day10part1, day10part2) where
import Text.Parsec (ParsecT, (<|>), space, parse, endOfLine, spaces, string, eof)
import Text.Parsec.Char (char)

day10part1 :: IO ()
day10part1 = do case parse theParser "" example of
                  Left pe -> error $ show pe
                  Right c -> print c

day10part2 :: IO ()
day10part2 = pure ()

data Character = Parenth | Square | Curly | Angle deriving (Eq)

type Stack = [Character]

push :: Stack -> Character -> Stack
push [] character = [character]
push ss character = ss ++ [character]

pop :: Stack -> Character -> Either Character Stack
pop [] character = Left character
pop ss character | head ss /= character = Left character
pop ss character | head ss == character = Right $ tail ss

-- parse :: String -> Either String (Character, String)
-- parse s:ss | s == "(" = (Parenth, ss)

parseParensInput :: (Monad m) => ParsecT String u m Char -> ParsecT String u m Char
parseParensInput p = char '(' <|> p <|> char ')' <|> p

parseSquareInput :: (Monad m) => ParsecT String u m Char -> ParsecT String u m Char
parseSquareInput p = char '[' <|> p <|> char ']' <|> p

parseAngleInput :: (Monad m) => ParsecT String u m Char -> ParsecT String u m Char
parseAngleInput p = char '<' <|> p <|> char '>' <|> p

parseCurlyInput :: (Monad m) => ParsecT String u m Char -> ParsecT String u m Char
parseCurlyInput p = char '{' <|> p <|> char '}'  <|> p

recurseParse :: (Monad m) => ParsecT String u m Char
recurseParse = parseAngleInput recurseParse <|> parseParensInput recurseParse <|> parseSquareInput recurseParse <|> parseCurlyInput recurseParse

theParser :: (Monad m) => ParsecT String u m Char
theParser = recurseParse  <* eof 


example = "()()"
-- parsensOpen :: Monad m => ParsecT [Char] u m Character
-- parsensOpen = Parenth <$ char '('

-- squareOpen :: Monad m => ParsecT [Char] u m Character
-- squareOpen = Parenth <$ char '['
