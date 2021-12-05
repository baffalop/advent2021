{-# LANGUAGE OverloadedStrings #-}

module Day05.Solution where

import Data.Attoparsec.Text (Parser)
import Data.Text (Text)
import qualified Data.Attoparsec.Text as P
import qualified Data.Map.Strict as Map
import Data.List.Extra (zipWithLongest)
import Data.Maybe (fromMaybe)

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
solveA = countOverlapping . foldMap enumerate . filter isPerpendicular

solveB :: [Line] -> Int
solveB = countOverlapping . foldMap enumerate

countOverlapping :: [Point] -> Int
countOverlapping = Map.size . Map.filter (> 1) . counts

counts :: Ord a => [a] -> Map.Map a Int
counts = foldr (flip (Map.insertWith (+)) 1) Map.empty

enumerate :: Line -> [Point]
enumerate ((x1, y1), (x2, y2)) =
  zipWithLongest (\xm ym -> (fromMaybe x1 xm, fromMaybe y1 ym)) (range x1 x2) (range y1 y2)

isPerpendicular :: Line -> Bool
isPerpendicular ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

range :: Int -> Int -> [Int]
range from to | from > to = [from, (from - 1)..to]
              | otherwise = [from..to]