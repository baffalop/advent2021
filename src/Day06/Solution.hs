module Day06.Solution (parse, solveA, solveB) where
  
import Data.Biapplicative (first)
import Data.Either.Extra (maybeToEither)
import Data.List (partition)
import Data.List.Extra (splitOn)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack)
import Text.Read (readMaybe)
import qualified Data.Map.Strict as Map

import Lib.Utils (counts)

type Generation = Map Phase Count
type Phase = Int
type Count = Integer

parse :: Text -> Either String Generation
parse = maybeToEither "Parse fail" .
  fmap counts . traverse readMaybe . splitOn "," . unpack

solveA :: Generation -> Integer
solveA = populationAfter 80

solveB :: Generation -> Integer
solveB = populationAfter 256

populationAfter :: Int -> Generation -> Integer
populationAfter n = sum . (!! n) . iterate simulate

simulate :: Generation -> Generation
simulate gen =
  let
    nextGen = Map.mapKeys (subtract 1) gen
  in fromMaybe nextGen $ do
    births <- Map.lookup (-1) nextGen
    pure
      $ Map.delete (-1)
      $ Map.insertWith (+) 6 births
      $ Map.insertWith (+) 8 births
      $ nextGen