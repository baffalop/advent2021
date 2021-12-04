{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}

module Day04.Solution (parse, solveA, solveB) where

import Data.List (transpose, partition)
import Data.Text (Text)
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as P
import qualified Data.Set as Set
import Data.Set (Set)
import Control.Monad (liftM2)
import Data.Function (on)
import Lib.Utils (safeHead, safeLast)

data Bingo = Bingo
  { called :: [Int]
  , boards :: [Board]
  }
  deriving Show

type Board = [[Int]]
type BoardSet = [Set Int]

parse :: Text -> Either String Bingo
parse = P.parseOnly $ Bingo
  <$> P.sepBy1 P.decimal (P.char ',')
  <*  P.skipSpace
  <*> P.sepBy1' board P.skipSpace
  where
    board :: Parser Board
    board =
      flip P.sepBy1' P.endOfLine $
        -- a board row might start with a padding space
        P.option () hSpace *>
        P.sepBy1' P.decimal (P.many1' hSpace)

    hSpace :: Parser ()
    hSpace = P.skip P.isHorizontalSpace

solveA :: Bingo -> Maybe Int
solveA = playAndScore safeHead

solveB :: Bingo -> Maybe Int
solveB = playAndScore safeLast

playAndScore :: (forall a . [a] -> Maybe a) -> Bingo -> Maybe Int
playAndScore pick Bingo{ called, boards } =
  fmap score $ pick $ play called $ fmap toSets boards

play :: [Int] -> [BoardSet] -> [(Int, BoardSet)]
play [] _ = []
play (called:next) boards =
  let
    marked = fmap (Set.delete called) <$> boards
    (winners, remaining) = partition (any Set.null) marked
  in fmap (called,) winners ++ play next remaining

score :: (Int, BoardSet) -> Int
score (winningNumber, unmarked) =
  -- the sets in unmarked include both rows and columns so will have duplicate numbers
  -- take the first half for only rows
  let unmarkedRows = take (length unmarked `div` 2) unmarked
  in winningNumber * sum (fmap sum unmarkedRows)

{-| The sets of rows *and* columns in the board -}
toSets :: Board -> BoardSet
toSets = liftM2 ((++) `on` fmap Set.fromList) id transpose