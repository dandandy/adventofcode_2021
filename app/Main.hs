module Main where

import Day2 (day2part1, day2part2)
import Day1 (day1part1, day1part2)
import Day3 (day3part1, day3part2)


main :: IO ()
main = sequence_ [
    day1part1, 
    day1part2, 
    day2part1, 
    day2part2, 
    day3part1, 
    day3part2
    ]