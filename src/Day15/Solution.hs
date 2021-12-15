{-# LANGUAGE NamedFieldPuns #-}
module Day15.Solution (parse, solveA, solveB) where

import Data.Text (Text, unpack)
import Data.Either.Extra (maybeToEither)
import Text.Read (readMaybe)
import Data.Tuple.Extra (both)
import Data.Biapplicative ((<<*>>))
import Data.Maybe (mapMaybe)
import Control.Arrow ((&&&))
import Data.List (nub, minimumBy)
import Data.Function (on)

type Cost = Int
type Plane = [[Cost]]
type Coord = (Int, Int)
  
data Path = Path
  { endpoint :: Coord
  , cost :: Cost
  }
  deriving (Eq, Show)
  
parse :: Text -> Either String Plane
parse = maybeToEither "parse fail " .
  traverse (traverse $ readMaybe . (:[])) . lines . unpack

solveA :: Plane -> Int
solveA = optimalPath

solveB :: Plane -> Int
solveB = undefined

optimalPath :: Plane -> Int
optimalPath plane = go [Path (0, 0) 0]
  where
    end :: Coord
    end = (length (head plane) - 1, length plane - 1)

    go :: [Path] -> Int
    go paths
      | all ((== end) . endpoint) paths = minimum $ cost <$> paths
      | otherwise =
      let
        next :: [Path]
        next = nub $ foldMap (extendOn plane) paths
        
        leastWorstCase :: Path
        leastWorstCase = minimumBy (compare `on` worstCaseCost end) next
      in
      go $ filter (canCatchUpTo leastWorstCase) next

extendOn :: Plane -> Path -> [Path]
extendOn plane Path{ endpoint, cost } =
  mapMaybe (\c -> Path c . (+ cost) <$> plane `costAt` c)
    $ neighbours endpoint

worstCaseCost :: Coord -> Path -> Int
worstCaseCost end Path{ endpoint, cost } =
  distance endpoint end * 9 + cost

canCatchUpTo :: Path -> Path -> Bool
canCatchUpTo target p = worstCaseCost (endpoint target) p <= cost target

neighbours :: Coord -> [Coord]
neighbours c = add c <$> [(1, 0), (-1, 0), (0, 1), (0, -1)]

distance :: Coord -> Coord -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

add :: Coord -> Coord -> Coord
add a b = both (+) a <<*>> b

costAt :: Plane -> Coord -> Maybe Cost
costAt plane (x, y) = plane !? y >>= (!? x)

(!?) :: [a] -> Int -> Maybe a
[] !? _ = Nothing
_ !? i | i < 0 = Nothing
(x:_) !? 0 = Just x
(_:xs) !? i = xs !? (i - 1)