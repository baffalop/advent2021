module Day03.Solution (parse, solveA, solveB) where

import Data.List (transpose, partition)
import Control.Arrow ((&&&))
import Data.Tuple.Extra (both)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

type Binary = String

parse :: String -> Either Never [Binary]
parse = Right . lines

solveA :: [Binary] -> Int
solveA = uncurry (*) . both toInt . (id &&& invert) . fmap mostCommonDigit . transpose

solveB :: [Binary] -> Int
solveB = undefined

mostCommonDigit :: Binary -> Char
mostCommonDigit ds =
  if (length $ filter (== '0') ds) >= (length ds `div` 2)
    then '0' else '1'

toInt :: Binary -> Int
toInt =
  sum . zipWith (*) (fmap (2^) [0..]) . reverse . mapMaybe (readMaybe . (:[]))

invert :: Binary -> Binary
invert = fmap (\bit -> if bit == '0' then '1' else '0')