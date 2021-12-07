module Day2 (day2part1, day2part2) where
import Text.Read (readMaybe)

data Action = Up Int | Forward Int | Down Int deriving (Show)
data Position = Position Int Int deriving (Show)

data PositionAim = PositionAim Int Int Int deriving (Show)

day2part1 :: IO ()
day2part1 = do x <- day2part1result
               putStrLn $ "Day 2 Part 1: " ++ x

day2part2 :: IO ()
day2part2 = do x <- day2part2result
               putStrLn $ "Day 2 Part 2: " ++ x

day2part1result :: IO String
day2part1result = do x <- readInputFile
                     return $ show $ multiplyPosition . calculatePosition <$> mapM readAction (lines x)

day2part2result :: IO String
day2part2result = do x <- readInputFile
                     return $ show $ multiplyPositionAim .  calculatePositionAim <$> mapM readAction (lines x)

example :: IO String
example = pure "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2\n"

readInputFile :: IO String 
readInputFile = readFile  "input2.txt"

multiplyPositionAim :: PositionAim -> Int
multiplyPositionAim (PositionAim a b _) = a * b

calculatePositionAim :: [Action] -> PositionAim
calculatePositionAim = foldl (flip applyActionToPositionAim) (PositionAim 0 0 0)

applyActionToPositionAim :: Action -> PositionAim -> PositionAim
applyActionToPositionAim (Forward i) (PositionAim horiz depth aim) =  PositionAim (horiz + i) (depth + i*aim) aim
applyActionToPositionAim (Up i) (PositionAim horiz depth aim) =       PositionAim horiz (depth ) (aim - i)
applyActionToPositionAim (Down i) (PositionAim horiz depth aim) =     PositionAim horiz (depth ) (aim + i)

multiplyPosition :: Position -> Int
multiplyPosition (Position a b) = a * b

calculatePosition :: [Action] -> Position
calculatePosition = foldl (flip applyActionToPosition) (Position 0 0)

applyActionToPosition :: Action -> Position -> Position
applyActionToPosition (Forward i) (Position horiz depth) =  Position (horiz + i) depth
applyActionToPosition (Up i) (Position horiz depth) =       Position horiz (depth - i)
applyActionToPosition (Down i) (Position horiz depth) =     Position horiz (depth + i)

readAction :: String -> Maybe Action
readAction s = readActionStart s <*> readActionEnd s

readActionStart :: String -> Maybe (Int -> Action)
readActionStart s | start == "forward" = Just Forward
                  | start == "down" = Just Down
                  | start == "up" = Just Up
                  | otherwise  = Nothing
    where start = reverse $ drop 2 $ reverse s

readActionEnd :: String -> Maybe Int
readActionEnd s = readMaybe $ drop (length s - 1) s