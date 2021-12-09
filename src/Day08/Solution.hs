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
  in
  applyExclusions firstPass

{-| Find sets of signals that are all mapped to the same set of outputs (eg. 'a' and 'c' are both possibly [ac])
  and exclude these sets from the other mappings (eg. 'd' cannot also be 'a' or 'c') -}
applyExclusions :: Wirings -> Wirings
applyExclusions wirings =
  let
    allSets :: [Set Char]
    allSets = snd <$> Map.toList wirings
    
    exclusiveSets :: [Set Char]
    exclusiveSets = nub $
      filter (\s -> frequency s allSets == Set.size s) allSets
  in
  (\s -> foldl' Set.difference s $ filter (/= s) exclusiveSets) <$> wirings

{-| Establish possible wirings based on which digits match that number of segments -}
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

frequency :: Eq a => a -> [a] -> Int
frequency x = length . filter (== x)