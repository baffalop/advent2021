module Day07.Solution (parse, solveA, solveB) where

import Data.List (sort)
import Data.Text (Text)
import qualified Data.Attoparsec.Text as P

parse :: Text -> Either String [Int]
parse = P.parseOnly $ P.decimal `P.sepBy1'` P.char ','

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