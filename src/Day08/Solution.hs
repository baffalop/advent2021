{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

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
  exclude $ foldr deduce (exclude firstPass) signals

{-| Find sets of signals that are all mapped to the same set of outputs (eg. 'a' and 'c' are both possibly [ac])
  and exclude these sets from the other mappings (eg. 'd' cannot also be 'a' or 'c') -}
exclude :: Wirings -> Wirings
exclude wirings =
  fmap
    (\s -> foldl' Set.difference s $ filter (/= s) $ exclusiveSets wirings)
    wirings

deduce :: Signal -> Wirings -> Wirings
deduce signal wirings =
  let
    exclusiveForSignal :: [Set Char]
    exclusiveForSignal =
      exclusiveSets $ Map.filterWithKey (const . (`elem` signal)) wirings

    fittingArrangements :: Set Char
    fittingArrangements = fold
      $ filter (\s -> all (`Set.isSubsetOf` s) exclusiveForSignal)
      $ possibleArrangements signal
  in
  foldr (Map.adjust $ Set.intersection fittingArrangements) wirings signal

exclusiveSets :: Wirings -> [Set Char]
exclusiveSets wirings =
  let
    allSets :: [Set Char]
    allSets = snd <$> Map.toList wirings
  in
  filter (\s -> frequency s allSets == Set.size s) allSets

{-| Establish possible wirings based on which digits match that number of segments -}
possibleWirings :: Signal -> Wirings
possibleWirings signal =
  let possibilities = fold $ possibleArrangements signal
  in Map.fromList $ (, possibilities) <$> signal

possibleArrangements :: Signal -> [Set Char]
possibleArrangements s = lookupAll (possibleDigits s) digitPositions

possibleDigits :: Signal -> [Digit]
possibleDigits s = fromMaybe [] $ Map.lookup (length s) segmentsDigits

obviously :: Signal -> Maybe Digit
obviously signal =
  case possibleDigits signal of
    [d] -> Just d
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

lookupAll :: Ord k => [k] -> Map k a -> [a]
lookupAll xs m = mapMaybe (`Map.lookup` m) xs

flipMap :: Ord a => Map k a -> Map a [k]
flipMap = Map.foldrWithKey (\k a -> Map.insertWith (<>) a [k]) Map.empty

frequency :: Eq a => a -> [a] -> Int
frequency x = length . filter (== x)