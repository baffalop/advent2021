{-# LANGUAGE NamedFieldPuns #-}

module Day02.Solution (parse, solveA, solveB) where

import Data.Biapplicative ((<<*>>))
import Data.Char (toUpper)
import Data.Either.Extra (maybeToEither)
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Data.Tuple.Extra (both)
import Text.Read (readMaybe)

data Direction = Up | Down | Forward
  deriving (Eq, Show, Read)

parse :: String -> Either String [(Direction, Int)]
parse = maybeToEither "Parse failed" . traverse (parseWords . words) . lines

parseWords :: [String] -> Maybe (Direction, Int)
parseWords [dir, n] = (,) <$> readMaybe (titleCase dir) <*> readMaybe n
parseWords _ = Nothing

solveA :: [(Direction, Int)] -> Int
solveA = uncurry (*) . foldr (add . move) (0, 0)
  where
    move :: (Direction, Int) -> (Int, Int)
    move (dir, n) = case dir of
      Up -> (0, -n)
      Down -> (0, n)
      Forward -> (n, 0)

data Velocity = Velocity
  { aim :: Int
  , coords :: (Int, Int)
  }

solveB :: [(Direction, Int)] -> Int
solveB = uncurry (*) . coords . foldl' move (Velocity 0 (0, 0))
  where
    move :: Velocity -> (Direction, Int) -> Velocity
    move v@Velocity{ aim, coords } (dir, n) = case dir of
      Up -> v{ aim = aim - n }
      Down -> v{ aim = aim + n }
      Forward -> v{ coords = add (n, n * aim) coords }

add :: Num n => (n, n) -> (n, n) -> (n, n)
add x y = both (+) x <<*>> y

titleCase :: String -> String
titleCase "" = ""
titleCase (first:rest) = toUpper first : rest