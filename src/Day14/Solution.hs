{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Day14.Solution where

import Data.Attoparsec.Text (Parser)
import Data.List (intersperse)
import Data.Map.Strict (Map)
import Data.MultiSet (MultiSet)
import Data.Text (Text)
import qualified Data.Attoparsec.Text as P
import qualified Data.Map.Strict as Map
import qualified Data.MultiSet as MS

import Lib.Utils (withConsecutive)

type Insertions = Map String Char

data Polymer = Polymer
  { template :: String
  , insertions :: Insertions
  }
  deriving (Show)

parse :: Text -> Either String Polymer
parse = P.parseOnly $ Polymer
  <$> word
  <*  P.skipSpace
  <*> (Map.fromList <$> rule `P.sepBy1'` P.endOfLine)
  where
    rule :: Parser (String, Char)
    rule = (,) <$> word <* P.string " -> " <*> P.letter

    word :: Parser String
    word = P.many1' P.letter

solveA :: Polymer -> Int
solveA Polymer{ template, insertions } =
  let
    resultCounts :: [Int]
    resultCounts =
      fmap snd $ MS.toOccurList $ MS.fromList
        $ iterate (insertWith insertions) template !! 10
  in
  maximum resultCounts - minimum resultCounts

insertWith :: Map String Char -> String -> String
insertWith insertions = foldMap insert . pairs
  where
    insert :: String -> String
    insert p = maybe p (`intersperse` p) $ Map.lookup p insertions

pairs :: [a] -> [[a]]
pairs = withConsecutive (\x y -> [x, y])

solveB :: Polymer -> Int
solveB = undefined

example :: Text
example =
  "NNCB\n\
  \\n\
  \CH -> B\n\
  \HH -> N\n\
  \CB -> H\n\
  \NH -> C\n\
  \HB -> C\n\
  \HC -> B\n\
  \HN -> C\n\
  \NN -> C\n\
  \BH -> H\n\
  \NC -> B\n\
  \NB -> B\n\
  \BN -> B\n\
  \BB -> N\n\
  \BC -> B\n\
  \CC -> N\n\
  \CN -> C"