{-# LANGUAGE OverloadedStrings #-}

module Day05.Solution where

import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as P
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Lib.Utils (counts)

type Point = (Int, Int)
type Line = (Point, Point)

parse :: Text -> Either String [Line]
parse = P.parseOnly $ line `P.sepBy1'` P.endOfLine
  where
    line :: Parser Line
    line = (,) <$> point <* P.string " -> " <*> point

    point :: Parser Point
    point = (,) <$> P.decimal <* P.char ',' <*> P.decimal

solveA :: [Line] -> Int
solveA = countOverlapping . foldMap points . filter isPerpendicular

solveB :: [Line] -> Int
solveB = countOverlapping . foldMap points

countOverlapping :: [Point] -> Int
countOverlapping = Map.size . Map.filter (> 1) . counts

points :: Line -> [Point]
points ((x1, y1), (x2, y2)) = zip (range x1 x2) (range y1 y2)

isPerpendicular :: Line -> Bool
isPerpendicular ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

{-| Range that can be descending. If from == to then it's infinite. -}
range :: Int -> Int -> [Int]
range from to = [from, from + signum (to - from)..to]