module Day06.Solution (parse, solveA, solveB) where
  
import Data.List.Extra (splitOn)
import Data.List (partition)
import Data.Text (Text, unpack)
import Data.Either.Extra (maybeToEither)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Data.Biapplicative (first)

parse :: Text -> Either String [Int]
parse = maybeToEither "Parse fail" . traverse readMaybe . splitOn "," . unpack

solveA :: [Int] -> Int
solveA = length . (!! 80) . iterate simulate

simulate :: [Int] -> [Int]
simulate = uncurry (<>) . first (foldMap $ const [6, 8]) . partition (== -1) . fmap (subtract 1)

solveB :: [Int] -> Int
solveB = length . (!! 256) . iterate simulate