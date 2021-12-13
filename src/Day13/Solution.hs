{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Day13.Solution (parse, solveA, solveB) where

import Data.Attoparsec.Text (Parser)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Set (Set)
import qualified Data.Attoparsec.Text as P
import qualified Data.Set as Set
import Data.Tuple.Extra (first, second, both)
import Data.List (nub, foldl', intercalate)

data Origami = Origami
  { plane :: [Coord]
  , folds :: [Fold]
  }
  deriving (Show)

type Coord = (Int, Int)
type Fold = (Axis, Int)

data Axis = Horizontal | Vertical
  deriving (Show)

parse :: Text -> Either String Origami
parse = P.parseOnly $ Origami <$> linesOf coord <* P.skipSpace <*> linesOf fold
  where
    coord :: Parser Coord
    coord = (,) <$> P.decimal <* P.char ',' <*> P.decimal

    fold :: Parser Fold
    fold = (,) <$ P.string "fold along "
      <*> P.choice [Horizontal <$ P.char 'x', Vertical <$ P.char 'y']
      <*  P.char '='
      <*> P.decimal

    linesOf :: Parser a -> Parser [a]
    linesOf p = p `P.sepBy1'` P.endOfLine

solveA :: Origami -> Int
solveA = length . (foldAlong <$> (head . folds) <*> plane)

solveB :: Origami -> String
solveB = render . (foldl' (flip foldAlong) <$> plane <*> folds)

foldAlong :: Fold -> [Coord] -> [Coord]
foldAlong (axis, n) = nub . fmap fold
  where
    fold :: Coord -> Coord
    fold c = if pick c < n then c else put ((+ n * 2) . negate) c

    pick :: Coord -> Int
    put :: (Int -> Int) -> Coord -> Coord
    (pick, put) = case axis of
      Horizontal -> (fst, first)
      Vertical -> (snd, second)

render :: [Coord] -> String
render plane = intercalate "\n"
  [ [ if (x, y) `elem` plane then '#' else '.'
      | x <- [0..maxX]
    ] | y <- [0..maxY]
  ]
  where (maxX, maxY) = (maximum . (<$> plane)) `both` (fst, snd)