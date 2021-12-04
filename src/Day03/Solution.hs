module Day03.Solution where

import Data.Function (on)
import Data.List (transpose, uncons)
import Control.Arrow ((&&&))
import Data.Tuple.Extra (both)
import Data.Maybe (mapMaybe, fromMaybe)
import Text.Read (readMaybe)
import Data.Text (Text, unpack)

type Binary = String
type Bit = Char

parse :: Text -> Either a [Binary]
parse = Right . lines . unpack

solveA :: [Binary] -> Int
solveA =
  multiply . (id &&& fmap bitFlip) . fmap mostCommonBit . transpose

solveB :: [Binary] -> Int
solveB input =
  multiply $ both (\f -> sieveByBitCriteria (f . mostCommonBit) input) (id, bitFlip)

mostCommonBit :: Binary -> Bit
mostCommonBit bits =
  if (frequency '0' bits) > (length bits `div` 2) then '0' else '1'

sieveByBitCriteria :: (Binary -> Bit) -> [Binary] -> Binary
sieveByBitCriteria criteria = sieve
  where
    sieve input = fromMaybe [] $ do
      unconsed <- traverse uncons input
      let matchingBit = criteria $ fmap fst unconsed
      case filter ((== matchingBit) . fst) unconsed of
        [] -> Nothing
        [(head, tail)] -> Just $ head : tail
        candidates -> Just $ matchingBit : sieve (fmap snd candidates)

multiply :: (Binary, Binary) -> Int
multiply = uncurry ((*) `on` decimal)

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