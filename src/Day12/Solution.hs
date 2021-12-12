{-# LANGUAGE OverloadedStrings #-}

module Day12.Solution (parse, solveA, solveB) where
  
import Data.Text (Text, unpack)
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as P
import qualified Data.Map as Map
import qualified Data.Char as Char

type Map = Map.Map Cave [Cave]

data Cave
  = Start
  | Big String
  | Small String
  | End
  deriving (Eq, Ord, Show)

parse :: Text -> Either String Map
parse = P.parseOnly $
  Map.fromListWith (<>) <$> tunnel `P.sepBy1'` P.endOfLine 
  where
    tunnel :: Parser (Cave, [Cave])
    tunnel = (,) <$> cave <* P.char '-' <*> ((:[]) <$> cave)
    
    cave :: Parser Cave
    cave = P.choice
      [ Start <$ P.string "start"
      , End <$ P.string "end"
      , Big <$> P.many1' (P.satisfy Char.isUpper)
      , Small <$> P.many1' (P.satisfy Char.isLower)
      ]

solveA :: Map -> Map
solveA = id

solveB :: Map -> Map
solveB = undefined