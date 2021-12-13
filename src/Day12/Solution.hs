{-# LANGUAGE OverloadedStrings #-}

module Day12.Solution (parse, solveA, solveB) where

import Data.Attoparsec.Text (Parser)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Tuple.Extra (second, swap)
import qualified Data.Attoparsec.Text as P
import qualified Data.Char as Char
import qualified Data.Map as Map

import Lib.Utils (frequency)

type CaveMap = Map.Map Cave [Cave]

data Cave
  = Start
  | Big String
  | Small String
  | End
  deriving (Eq, Ord, Show)

parse :: Text -> Either String CaveMap
parse = P.parseOnly $
  Map.fromListWith (nub ... (<>)) . bidirectional <$> link `P.sepBy1'` P.endOfLine
  where
    link :: Parser (Cave, Cave)
    link = (,) <$> cave <* P.char '-' <*> cave

    cave :: Parser Cave
    cave = P.choice
      [ Start <$ P.string "start"
      , End <$ P.string "end"
      , Big <$> P.many1' (P.satisfy Char.isUpper)
      , Small <$> P.many1' (P.satisfy Char.isLower)
      ]
    
    bidirectional :: [(Cave, Cave)] -> [(Cave, [Cave])]
    bidirectional = (<>) <$> fmap (second (:[])) <*> fmap (second (:[]) . swap)

solveA :: CaveMap -> Int
solveA = length . paths (const False)

solveB :: CaveMap -> Int 
solveB = length . paths ((\p -> nub p == p) . filter isSmall)

paths :: ([Cave] -> Bool) -> CaveMap -> [[Cave]]
paths canAddSmall map = explore [] Start
  where
    explore :: [Cave] -> Cave -> [[Cave]]
    explore path cave = case cave of
      End -> [End : path]
      Start | Start `elem` path -> []
      (Small _) | not (canAddSmall path) && cave `elem` path -> []
      _ -> fromMaybe []
        $ foldMap (explore $ cave : path)
        <$> Map.lookup cave map

isSmall :: Cave -> Bool
isSmall (Small _) = True
isSmall _ = False

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) . (.)