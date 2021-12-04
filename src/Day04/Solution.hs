{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Day04.Solution where

import Data.List (transpose, foldl', find)
import Data.Text (Text)
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as P
import qualified Data.Set as Set
import Data.Set (Set)
import Control.Monad (liftM2)
import Data.Function (on)

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
solveA Bingo{ called, boards } = do
  (winningNumber, unmarked) <- play called $ fmap toSets boards
  -- the sets in unmarked include both rows and columns so will have duplicate numbers
  -- take the first half for only rows
  let unmarkedRows = take (length unmarked `div` 2) unmarked
  pure $ winningNumber * sum (fmap sum unmarkedRows)

play :: [Int] -> [BoardSet] -> Maybe (Int, BoardSet)
play [] _ = Nothing
play (called:next) boards =
  let marked = fmap (Set.delete called) <$> boards
  in case find (any Set.null) marked of
       Nothing -> play next marked
       Just winner -> Just (called, winner)

{-| The sets of rows *and* columns in the board -}
toSets :: Board -> BoardSet
toSets = liftM2 ((++) `on` fmap Set.fromList) id transpose

-- solveB :: Bingo -> Maybe Int
solveB = undefined