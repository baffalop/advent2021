module Day03.Solution (parse, solveA, solveB) where

import Data.List (transpose, partition)
import Control.Arrow ((&&&))
import Data.Tuple.Extra (both)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

parse :: String -> Either a [String]
parse = Right . lines

solveA :: [String] -> Int
solveA = uncurry (*) . both toInt . (id &&& invert) . fmap mostCommonDigit . transpose

solveB :: [String] -> Int
solveB = undefined

mostCommonDigit :: String -> Char
mostCommonDigit ds =
  if (length $ filter (== '0') ds) >= (length ds `div` 2)
    then '0' else '1'

toInt :: String -> Int
toInt =
  sum . zipWith (*) (fmap (2^) [0..]) . reverse . mapMaybe (readMaybe . (:[]))

invert :: String -> String
invert = fmap (\bit -> if bit == '0' then '1' else '0')