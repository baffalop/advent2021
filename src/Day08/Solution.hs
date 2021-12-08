{-# LANGUAGE OverloadedStrings #-}

module Day08.Solution (parse, solveA, solveB) where
  
import Data.Text (Text)
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as P

type Signal = String

data Display = Display
  { inputs :: [Signal] 
  , output :: [Signal]
  }
  deriving Show

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
solveA = undefined

solveB :: [Display] -> Int
solveB = undefined