module Main where

import Day5 (Line (Line), Point (Point), getDiagLinePoints)
import qualified Day5
import Test.HUnit


main = runTestTTAndExit $ test ["test1" ~: [Point 0 0, Point 1 1] ~=? Day5.getDiagLinePoints (Line (Point 0 0) (Point 1 1))]

test1 :: Test
test1 = TestCase $ assertEqual "should get points to top left" [Point 0 1] (Day5.getDiagLinePoints (Line (Point 0 0) (Point 1 1)))