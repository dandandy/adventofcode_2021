module Main where

import Day2 (day2part1, day2part2)
import Day1 (day1part1, day1part2)
import Day3 (day3part1, day3part2)
import Day4 (day4part1, day4part2)
import Day5 (day5part1, day5part2)
import Day6 (day6part1, day6part2)
import Day7 (day7part1, day7part2)
import Day8
import Day9
import Day10
import Day11
import Day12

import MaybeTExample

import Day13
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT, MaybeT))
import Text.Read (readMaybe)
import Control.Monad.Cont (MonadTrans(lift), MonadIO (liftIO))

main :: IO ()
main = sequence_ [
    -- day1part1, 
    -- day1part2, 
    -- day2part1, 
    -- day2part2, 
    -- day3part1, 
    -- day3part2,
    -- day4part1, 
    -- day4part2,
    -- day5part1,
    -- day5part2,
    -- day6part1,
    -- day6part2,
    -- day7part1 ,
    -- day7part2,
    -- day8p1,
    -- day8p2
    -- day9part1,
    -- day9part2
    -- day10part1,
    -- day10part2
    -- day11part1,
    -- day11part2
    -- day12part1,
    -- day12part2,
    -- day13part1,
    -- day13part2
    MaybeTExample.example 
    ]
