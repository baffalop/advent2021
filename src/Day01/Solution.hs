module Day01.Solution (parse, solveA, solveB) where

import Data.List (tails)
import Data.List.Extra (dropEnd)
import Lib.Utils (withConsecutive)
    
parse :: String -> [Int]
parse = fmap read . lines

solveA :: [Int] -> Int
solveA = length . filter (< 0) . withConsecutive (-)

solveB :: [Int] -> Int
solveB = solveA . fmap sum . rollingWindows 3

rollingWindows :: Int -> [a] -> [[a]]
rollingWindows n = fmap (take n) . dropEnd (n - 1) . tails