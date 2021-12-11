module Day10.Solution (parse, solveA, solveB) where

import Data.Text (Text, unpack)
import Data.Maybe (mapMaybe)
import Data.List (elemIndex, foldl', sort)
import Data.Tuple.Extra (both)
import Data.Either (lefts, rights)
  
parse :: Text -> Either a [String]
parse = Right . lines . unpack

solveA :: [String] -> Int
solveA = sum . fmap errorScore . lefts . fmap syntaxCheck

solveB :: [String] -> Int
solveB = median . fmap completionScore . rights . fmap syntaxCheck

syntaxCheck :: String -> Either Char String
syntaxCheck = check []
  where
    check :: String -> String -> Either Char String
    check stack "" = Right stack
    check stack (c:next) =
      case (elemIndex c openings, stack) of
        (Just i, _) -> check (closings !! i : stack) next
        (Nothing, s : rest) | s == c -> check rest next
        _ -> Left c

median :: [Int] -> Int
median ns = sort ns !! (length ns `div` 2)

completionScore :: String -> Int
completionScore = foldl' (\s c -> closeScore c + 5 * s) 0

closeScore :: Char -> Int
closeScore ')' = 1
closeScore ']' = 2
closeScore '}' = 3
closeScore '>' = 4
closeScore _ = 0

errorScore :: Char -> Int
errorScore ')' = 3
errorScore ']' = 57
errorScore '}' = 1197
errorScore '>' = 25137
errorScore _ = 0

openings :: [Char]
openings = "({[<"

closings :: [Char]
closings = ")}]>"