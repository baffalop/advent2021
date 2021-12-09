{-# LANGUAGE OverloadedStrings #-}

module Day08.Solution (parse, solveA, solveB) where

import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Attoparsec.Text (Parser)
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Attoparsec.Text as P
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Biapplicative (Bifunctor(second))
import Data.Bifunctor (bimap)

type Signal = String

data Display = Display
  { inputs :: [Signal]
  , output :: [Signal]
  }
  deriving Show

newtype Digit = Digit { fromDigit :: Int }
  deriving (Eq, Ord)

parse :: Text -> Either String [Display]
parse = P.parseOnly $ display `P.sepBy1'` P.endOfLine
  where
    display :: Parser Display
    display = Display
      <$> signals
      <*  P.string " | "
      <*> signals

    signals :: Parser [Signal]
    signals = signal `P.sepBy1'` P.char ' '

    signal :: Parser Signal
    signal = P.many1' $ P.satisfy $ P.inClass "abcdefg"

solveA :: [Display] -> Int
solveA = sum . fmap (length . mapMaybe obviously . output)

solveB :: [Display] -> Int
solveB = undefined

obviously :: Signal -> Maybe Digit
obviously s =
  case Map.lookup (length s) segmentsDigits of
    Just [d] -> Just d
    _ -> Nothing

segmentsDigits :: Map Int [Digit]
segmentsDigits = flipMap $ Set.size <$> digitPositions

digitPositions :: Map Digit (Set Char)
digitPositions = Map.fromList $ bimap Digit Set.fromList <$>
  [ (0, "abcefg")
  , (1, "cf")
  , (2, "acdeg")
  , (3, "acdfg")
  , (4, "bcdf")
  , (5, "abdfg")
  , (6, "abdefg")
  , (7, "acf")
  , (8, "abcdefg")
  , (9, "abcdfg")
  ]

flipMap :: Ord a => Map k a -> Map a [k]
flipMap = Map.foldrWithKey (\k a -> Map.insertWith (<>) a [k]) Map.empty