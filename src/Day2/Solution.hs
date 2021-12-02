{-# LANGUAGE NamedFieldPuns #-}

module Day2.Solution (parse, solveA, solveB) where

import Text.Read (readMaybe)
import Data.List.Extra (upper)
import Data.Char (toUpper)
import Data.Biapplicative ((<<*>>))
import Data.Tuple.Extra (both)
import Data.Maybe (fromMaybe)
import Data.Foldable (foldl')

data Direction = Up | Down | Forward
  deriving (Eq, Show, Read)

parse :: String -> [(Direction, Int)]
parse = fromMaybe (error "Parse failed") . traverse (parseWords . words) . lines

parseWords :: [String] -> Maybe (Direction, Int)
parseWords [dir, n] = (,) <$> readMaybe (titleCase dir) <*> readMaybe n
parseWords _ = Nothing

solveA :: [(Direction, Int)] -> Int
solveA = uncurry (*) . foldr ((<<*>>) . both (+) . move) (0, 0)
  where
    move :: (Direction, Int) -> (Int, Int)
    move (dir, n) = case dir of
      Up -> (0, -n)
      Down -> (0, n)
      Forward -> (n, 0)

data Velocity = Velocity
  { coords :: (Int, Int)
  , aim :: Int
  }

solveB :: [(Direction, Int)] -> Int
solveB = uncurry (*) . coords . foldl' move (Velocity (0, 0) 0)
  where
    move :: Velocity -> (Direction, Int) -> Velocity
    move v@Velocity{ aim, coords } (dir, n) = case dir of
      Up -> v{ aim = aim - n }
      Down -> v{ aim = aim + n }
      Forward -> v{ coords = both (+) (n, n * aim) <<*>> coords }

titleCase :: String -> String
titleCase "" = ""
titleCase (first:rest) = toUpper first : rest