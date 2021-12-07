module Day07.Solution (parse, solveA, solveB) where

import Text.Read (readMaybe)
import Data.List.Extra (splitOn)
import Data.Text (Text, unpack)
import Data.Either.Extra (maybeToEither)
import Data.List (sort)

parse :: Text -> Either String [Int]
parse = maybeToEither "Parse fail" .  traverse readMaybe . splitOn "," . unpack

solveA :: [Int] -> Int
solveA positions = sum $ fmap (abs . (median positions -)) positions

solveB :: [Int] -> Int
solveB positions = sum $ fmap (triangular . abs . (mean positions -)) positions

median :: [Int] -> Int
median xs = sort xs !! (length xs `div` 2)

mean :: [Int] -> Int
mean xs = sum xs `div` length xs

triangular :: Int -> Int
triangular n = n * (n + 1) `div` 2