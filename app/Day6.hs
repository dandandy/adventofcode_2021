-- {-# LANGUInt DeriveAnyClass #-}
module Day6 (day6part1, day6part2) where

import Text.Parsec
import Text.Read
import Debug.Trace
import Text.Parsec.String (parseFromFile)

day6part1 :: IO ()
day6part1 = do  
    x <- parseFromFile readInput "input6.txt"
    case x of
        Left pe -> error $ show pe
        Right m_ns -> trace (show m_ns) $ print (length . simulate <$> m_ns)

day6part2 :: IO ()
day6part2 = pure ()

example :: String
example = "3,4,3,1,2"

day :: Int -> [Int]
day 0 = [6, 8]
day n = [n - 1]

simulate :: [Int] -> [Int]
simulate n = trace (show n) $ iterate (concatMap day) n !! 80

readInput :: (Monad m) => ParsecT String u m (Maybe [Int])
readInput = mapM readMaybe <$> many1 digit `sepBy` char ','
