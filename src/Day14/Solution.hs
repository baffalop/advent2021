{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Day14.Solution where

import Data.Attoparsec.Text (Parser)
import Data.Bifunctor (second, bimap)
import Data.Function ((&))
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe)
import Data.MultiSet (MultiSet)
import Data.Text (Text)
import qualified Data.Attoparsec.Text as P
import qualified Data.Map.Strict as Map
import qualified Data.MultiSet as MS

import Lib.Utils (withConsecutive, linesOf)

type Rules = Map String Char

data Polymer = Polymer
  { template :: String
  , rules :: Rules
  }
  deriving (Show)

data Process = Process
  { elements :: MultiSet Char
  , next :: MultiSet String
  }

parse :: Text -> Either String Polymer
parse = P.parseOnly $ Polymer
  <$> word
  <*  P.skipSpace
  <*> (Map.fromList <$> linesOf rule)
  where
    rule :: Parser (String, Char)
    rule = (,) <$> word <* P.string " -> " <*> P.letter

    word :: Parser String
    word = P.many1' P.letter

solveA :: Polymer -> Int
solveA = solve 10

solveB :: Polymer -> Int
solveB = solve 40

solve :: Int -> Polymer -> Int
solve n Polymer{ template, rules } =
  let
    resultCounts :: [Int]
    resultCounts =
      iterate (insertWith rules) (initialise template) !! n
        & elements & MS.toOccurList & fmap snd
  in
  maximum resultCounts - minimum resultCounts

initialise :: String -> Process
initialise template =
  Process
    { elements = MS.fromList template
    , next = MS.fromList $ pairs template
    }

insertWith :: Rules -> Process -> Process
insertWith rules Process{ elements, next } =
  let
    addedElements :: [(Char, Int)]
    subsequent :: [(String, Int)]
    (addedElements, subsequent) =
      MS.toOccurList next
        & mapMaybe (\(i, n) -> bimap (,n) (,n) <$> expandWith rules i)
        & unzip & second (foldMap rewrap)
  in
  Process
    { elements = MS.union elements $ MS.fromOccurList addedElements
    , next = MS.fromOccurList subsequent
    }

expandWith :: Rules -> String -> Maybe (Char, [String])
expandWith rules s@[x, y] = do
  inserted <- Map.lookup s rules
  let next = filter (`Map.member` rules) [[x, inserted], [inserted, y]]
  pure (inserted, next)
expandWith _ _ = Nothing

rewrap :: ([a], b) -> [(a, b)]
rewrap (xs, y) = (,y) <$> xs

pairs :: [a] -> [[a]]
pairs = withConsecutive (\x y -> [x, y])