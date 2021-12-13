{-# LANGUAGE OverloadedStrings #-}

module Day12.Solution where

import Data.Text (Text)
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as P
import qualified Data.Map as Map
import qualified Data.Char as Char
import Data.Maybe (fromMaybe)
import Data.List (nub)
import Data.Tuple.Extra (second, swap)

type CaveMap = Map.Map Cave [Cave]

data Cave
  = Start
  | Big String
  | Small String
  | End
  deriving (Eq, Ord, Show)

parse :: Text -> Either String CaveMap
parse = P.parseOnly $
  Map.fromListWith (nub ... (<>)) . prepare <$> link `P.sepBy1'` P.endOfLine
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
    
    prepare :: [(Cave, Cave)] -> [(Cave, [Cave])]
    prepare = (<>) <$> fmap (second (:[])) <*> fmap (second (:[]) . swap)

solveA :: CaveMap -> Int
solveA = length . paths (const False)

solveB :: CaveMap -> Int 
solveB = length . paths ((\p -> nub p == p) . filter isSmall)

paths :: ([Cave] -> Bool) -> CaveMap -> [[Cave]]
paths canAddSmall map = explore [Start]
  where
    explore :: [Cave] -> [[Cave]]
    explore [] = error "Empty path"
    explore path@(current:prev) =
      case current of
        End -> [path]
        Start | Start `elem` prev -> []
        c@(Small _) | not (canAddSmall prev) && c `elem` prev -> []
        c -> fromMaybe []
          $ foldMap (explore . (: path))
          <$> Map.lookup c map

isSmall :: Cave -> Bool
isSmall (Small _) = True
isSmall _ = False

frequency :: Eq a => a -> [a] -> Int
frequency x = length . filter (== x)

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) . (.)