module Main where

import Day2 (day2)
import Day1 (day1part1, day1part2)


main :: IO ()
main = sequence_ [day1part1, day1part2]