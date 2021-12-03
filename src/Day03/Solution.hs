module Day03.Solution where

import Data.Function (on)
import Data.List (transpose, partition, maximumBy)
import Control.Arrow ((&&&))
import Data.Tuple.Extra (both)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

type Binary = String
type Bit = Char

parse :: String -> Either a [Binary]
parse = Right . lines

solveA :: [Binary] -> Int
solveA = uncurry multDecimal . (id &&& fmap bitFlip) . fmap mostCommonBit . transpose

solveB :: [Binary] -> Int
solveB input =
  uncurry multDecimal $ both (\f -> matchByBitCriteria (f . mostCommonBit) input) (id, bitFlip)

matchByBitCriteria :: (Binary -> Bit) -> [Binary] -> Binary
matchByBitCriteria criteria input =
  maximumBy (compare `on` (prefixMatch $ fmap criteria $ transpose input)) input

prefixMatch :: Eq a => [a] -> [a] -> Int
prefixMatch (x:xs) (y:ys) | x == y = 1 + prefixMatch xs ys
prefixMatch _ _ = 0

mostCommonBit :: Binary -> Bit
mostCommonBit ds =
  if (frequency '0' ds) >= (length ds `div` 2)
    then '0' else '1'

multDecimal :: Binary -> Binary -> Int
multDecimal = (*) `on` decimal

decimal :: Binary -> Int
decimal =
  sum . zipWith (*) (powersOf 2) . reverse . mapMaybe (readMaybe . (:[]))

bitFlip :: Bit -> Bit
bitFlip '0' = '1'
bitFlip _ = '0'

powersOf :: Int -> [Int]
powersOf x = fmap (x^) [0..]

frequency :: Eq a => a -> [a] -> Int
frequency x = length . filter (== x)