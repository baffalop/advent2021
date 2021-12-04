module Day04.Solution (parse, solveA, solveB) where

import Data.Text (Text)
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as P
  
data Bingo = Bingo
  { called :: [Int]
  , boards :: [Board]
  }
  deriving Show

type Board = [[Int]]

parse :: Text -> Either String Bingo
parse = P.parseOnly $ Bingo
  <$> P.sepBy1 P.decimal (P.char ',')
  <*  P.skipSpace
  <*> P.sepBy1' board P.skipSpace
  where
    board :: Parser Board
    board =
      flip P.sepBy1' P.endOfLine $
        P.sepBy1' P.decimal (P.many1' $ P.satisfy P.isHorizontalSpace)
    
solveA :: Bingo -> Bingo
solveA = id
  
solveB :: Bingo -> Bingo
solveB = undefined