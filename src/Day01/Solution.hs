module Day01.Solution (parse, solveA, solveB) where

import Data.Either.Extra (maybeToEither)
import Data.List (tails)
import Data.List.Extra (dropEnd)
import Lib.Utils (withConsecutive)
import Text.Read (readMaybe)
    
parse :: String -> Either String [Int]
parse = maybeToEither "Non-integer in input" . traverse readMaybe . lines

solveA :: [Int] -> Int
solveA = length . filter (< 0) . withConsecutive (-)

solveB :: [Int] -> Int
solveB = solveA . fmap sum . rollingWindows 3

rollingWindows :: Int -> [a] -> [[a]]
rollingWindows n = fmap (take n) . dropEnd (n - 1) . tails