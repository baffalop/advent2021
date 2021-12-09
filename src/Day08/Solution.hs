{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day08.Solution (parse, solveA, solveB) where

import Data.Maybe (mapMaybe, fromMaybe)
import Data.Text (Text)
import Data.Attoparsec.Text (Parser)
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Attoparsec.Text as P
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Foldable (fold, Foldable (foldl'))
import Data.Biapplicative (bimap)
import Data.List (nub)

type Signal = String
type Wirings = Map Char (Set Char)

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

solveB :: [Display] -> [Map Char String]
solveB = fmap (fmap Set.toList . resolveWirings . inputs)

resolveWirings :: [Signal] -> Wirings
resolveWirings signals =
  let
    firstPass :: Wirings
    firstPass = Map.unionsWith Set.intersection $ possibleWirings <$> signals

    exclusiveSets :: [Set Char] 
    exclusiveSets = exclusive firstPass
  in
  fmap (\s -> foldl' Set.difference s $ filter (/= s) exclusiveSets) firstPass

exclusive :: Wirings -> [Set Char]
exclusive wirings =
  let
    allSets = snd <$> Map.toList wirings
  in
  nub $ filter (\s -> length (filter (== s) allSets) == Set.size s) allSets

possibleWirings :: Signal -> Wirings
possibleWirings signal = fromMaybe Map.empty $ do
  digits <- Map.lookup (length signal) segmentsDigits
  let positions = lookupAll digits digitPositions
  pure $ Map.fromList $ (, positions) <$> signal

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

lookupAll :: (Ord k, Monoid a) => [k] -> Map k a -> a
lookupAll xs m = fold $ mapMaybe (`Map.lookup` m) xs

flipMap :: Ord a => Map k a -> Map a [k]
flipMap = Map.foldrWithKey (\k a -> Map.insertWith (<>) a [k]) Map.empty