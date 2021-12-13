{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day08.Solution (parse, solveA, solveB) where

import Data.Function ((&))
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Text (Text)
import Data.Attoparsec.Text (Parser)
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Attoparsec.Text as P
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Foldable (fold, Foldable (foldl'), find)
import Data.Biapplicative (bimap)

import Lib.Utils (frequency)

type Signal = String
type Wirings = Map Char (Set Char)

data Display = Display
  { inputs :: [Signal]
  , output :: [Signal]
  }
  deriving Show

newtype Digit = Digit { fromDigit :: Int }
  deriving (Eq, Ord, Show)

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
solveB = sum . fmap (digitsToInt . (fmap . decode <$> (solveWirings . inputs) <*> output))

solveWirings :: [Signal] -> Wirings
solveWirings signals =
  possibleWirings <$> signals
    & Map.unionsWith Set.intersection 
    & iterate (flip (foldr deduce) signals . exclude)
    & dropWhile (any $ (> 1) . Set.size)
    & head

decode :: Wirings -> Signal -> Digit
decode wirings signal =
  Map.toList digitPositions
    & find ((== fold (lookupAll signal wirings)) . snd)
    & maybe (Digit $ -1) fst

digitsToInt :: [Digit] -> Int
digitsToInt = foldl' (\s d -> fromDigit d + s * 10) 0

{-| Find sets of signals that are all mapped to the same set of outputs (eg. 'a' and 'c' are both possibly [ac])
  and exclude these sets from the other mappings (eg. 'd' cannot also be 'a' or 'c') -}
exclude :: Wirings -> Wirings
exclude wirings =
  fmap
    (\s -> foldl' Set.difference s $ filter (/= s) $ exclusiveSets wirings)
    wirings

{-| For a given signal, if it contains an exclusive set of wirings, the members of that set
  must be present in the output for that signal. Use this to reduce possible wirings for that signal. -}
deduce :: Signal -> Wirings -> Wirings
deduce signal wirings =
  let
    exclusiveForSignal :: [Set Char]
    exclusiveForSignal =
      exclusiveSets $ Map.filterWithKey (const . (`elem` signal)) wirings

    fittingArrangements :: Set Char
    fittingArrangements =
      possibleArrangements signal
        & filter (\s -> all (`Set.isSubsetOf` s) exclusiveForSignal)
        & fold
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