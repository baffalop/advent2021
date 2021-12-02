module Day2.Solution (parse, solveA, solveB) where

import Text.Read (readMaybe)
import Data.List.Extra (upper)
import Data.Char (toUpper)
import Data.Biapplicative ((<<*>>))
import Data.Tuple.Extra (both)
import Data.Maybe (fromMaybe)

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

solveB :: [(Direction, Int)] -> Int
solveB = undefined

titleCase :: String -> String
titleCase "" = ""
titleCase (first:rest) = toUpper first : rest