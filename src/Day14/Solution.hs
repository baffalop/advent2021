{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Day14.Solution where

import Data.Attoparsec.Text (Parser)
import Data.Bifunctor (second)
import Data.Function ((&))
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe)
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

data Process = Process
  { elements :: MultiSet Char
  , next :: MultiSet String
  }

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
solveA p@Polymer{ template, insertions } =
  let
    resultCounts :: [Int]
    resultCounts =
      iterate (insertWith insertions) (initialise p) !! 10
        & elements & MS.toOccurList & fmap snd
  in
  maximum resultCounts - minimum resultCounts

initialise :: Polymer -> Process
initialise Polymer{ template, insertions } =
  Process
    { elements = MS.fromList template
    , next = MS.fromList $ pairs template
    }

insertWith :: Insertions -> Process -> Process
insertWith insertions Process{ elements, next } =
  let
    addedElements :: [(Char, Int)]
    subsequent :: [(String, Int)]
    (addedElements, subsequent) =
      MS.toOccurList next
        & mapMaybe (\(i, n) -> (,n) <$> expandWith insertions i)
        & unzipWithOccurs
        & second (foldMap unpackOccurs)
  in
  Process
    { elements = MS.union elements $ MS.fromOccurList addedElements
    , next = MS.fromOccurList subsequent
    }

expandWith :: Insertions -> String -> Maybe (Char, [String])
expandWith insertions s@[x, y] = do
  inserted <- Map.lookup s insertions
  let next = filter (`Map.member` insertions) [[x, inserted], [inserted, y]]
  pure (inserted, next)
expandWith _ _ = Nothing

unzipWithOccurs :: [((a, b), Int)] -> ([(a, Int)], [(b, Int)])
unzipWithOccurs = unzip . fmap (\((x, y), n) -> ((x, n), (y, n)))

unpackOccurs :: ([a], Int) -> [(a, Int)]
unpackOccurs (xs, n) = (,n) <$> xs

pairs :: [a] -> [[a]]
pairs = withConsecutive (\x y -> [x, y])

solveB :: Polymer -> Int
solveB = undefined