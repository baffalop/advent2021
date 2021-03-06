module Day06.Solution (parse, solveA, solveB) where
  
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Attoparsec.Text as P
import qualified Data.Map.Strict as Map

import Lib.Utils (counts)

type Generation = Map Phase Count
type Phase = Int
type Count = Integer

parse :: Text -> Either String Generation
parse = P.parseOnly $ fmap counts $ P.decimal `P.sepBy1'` P.char ','

solveA :: Generation -> Integer
solveA = populationAfter 80

solveB :: Generation -> Integer
solveB = populationAfter 256

populationAfter :: Int -> Generation -> Integer
populationAfter n = sum . (!! n) . iterate generation

generation :: Generation -> Generation
generation gen =
  let
    nextGen = Map.mapKeys (subtract 1) gen
  in fromMaybe nextGen $ do
    births <- Map.lookup (-1) nextGen
    pure
      $ Map.delete (-1)
      $ Map.insertWith (+) 6 births
      $ Map.insertWith (+) 8 births
      $ nextGen