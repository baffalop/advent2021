{-# LANGUAGE OverloadedStrings #-}

module Day05.Solution (parse, solveA, solveB) where

import Data.Attoparsec.Text (Parser)
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Attoparsec.Text as P
import qualified Data.Map.Strict as Map

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
solveB = undefined

countOverlapping :: [Point] -> Int
countOverlapping = Map.size . Map.filter (> 1) . counts

counts :: Ord a => [a] -> Map.Map a Int
counts = foldr (flip (Map.insertWith (+)) 1) Map.empty

enumerate :: Line -> [Point]
enumerate ((x1, y1), (x2, y2)) = [(x, y) | x <- [x1..x2], y <- [y1..y2]]

isPerpendicular :: Line -> Bool
isPerpendicular ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2