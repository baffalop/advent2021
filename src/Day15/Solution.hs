module Day15.Solution (parse, solveA, solveB) where

import Data.Text (Text, unpack)
import Data.Either.Extra (maybeToEither)
import Text.Read (readMaybe)

type Grid = [[Int]]
  
parse :: Text -> Either String Grid
parse = maybeToEither "parse fail " .
  traverse (traverse $ readMaybe . (:[])) . lines . unpack

solveA :: Grid -> Grid
solveA = id

solveB :: Grid -> Int
solveB = undefined