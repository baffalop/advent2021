{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Day13.Solution (parse, solveA, solveB) where

import Data.Attoparsec.Text (Parser)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Set (Set)
import qualified Data.Attoparsec.Text as P
import qualified Data.Set as Set

data Origami = Origami
  { plane :: Set Coord
  , folds :: [Fold]
  }
  deriving (Show)

type Coord = (Int, Int)
type Fold = (Orientation, Int)

data Orientation = Horizontal | Vertical
  deriving (Show)

parse :: Text -> Either String Origami
parse = P.parseOnly $ Origami <$> plane <* P.skipSpace <*> linesOf fold
  where
    plane :: Parser (Set Coord)
    plane = Set.fromList <$> linesOf coord
    
    coord :: Parser Coord
    coord = (,) <$> P.decimal <* P.char ',' <*> P.decimal

    fold :: Parser Fold
    fold = (,) <$ P.string "fold along "
      <*> P.choice [Horizontal <$ P.char 'x', Vertical <$ P.char 'y']
      <*  P.char '='
      <*> P.decimal
    
    linesOf :: Parser a -> Parser [a]
    linesOf p = p `P.sepBy1'` P.endOfLine 

solveA :: Origami -> Origami
solveA = id

solveB :: Origami -> Int
solveB = undefined