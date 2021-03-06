module Day01.Solution (parse, solveA, solveB) where

import Data.Either.Extra (maybeToEither)
import Data.List (tails)
import Data.List.Extra (dropEnd)
import Text.Read (readMaybe)
import Data.Text (Text, unpack)
    
parse :: Text -> Either String [Int]
parse = maybeToEither "Non-integer in input" . traverse readMaybe . lines . unpack

solveA :: [Int] -> Int
solveA = length . filter (< 0) . withConsecutive (-)

solveB :: [Int] -> Int
solveB = solveA . fmap sum . rollingWindows 3

withConsecutive :: (a -> a -> b) -> [a] -> [b]
withConsecutive f xs = zipWith f xs (drop 1 xs)

rollingWindows :: Int -> [a] -> [[a]]
rollingWindows n = fmap (take n) . dropEnd (n - 1) . tails