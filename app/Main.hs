{-# LANGUAGE NamedFieldPuns, LambdaCase #-}

module Main where
    
import Control.Monad (when, guard)
import Data.Either.Extra (maybeToEither)
import Data.Maybe (catMaybes)
import Data.Tuple.Extra (both)
import Options.Applicative (help, long, metavar, short)
import Text.Read (readMaybe)
import qualified Advent
import qualified Options.Applicative as Opt
import Data.Text (Text, unpack)

import Lib.Utils (maybeIf)
import qualified Day01.Solution as Day01
import qualified Day02.Solution as Day02
import qualified Day03.Solution as Day03

data Options = Options
  { day :: Day
  , parts :: [DayPart]
  , key :: SessionKey
  }
  deriving (Show)

type Day = Int

data DayPart = PartA | PartB
  deriving (Eq, Show)

newtype SessionKey = Key String

instance Show SessionKey where
  show (Key k) = k

type Solution = String -> Either ParseError (String, String)
type ParseError = String

main :: IO ()
main = do
  options@Options{ day, parts } <- Opt.execParser cli

  input <- fetchInput options

  when (length input < 20) $
    fail "Input is suspiciously small. Are you sure you piped the right thing?"

  case solutionsFor day input of
    Left e -> fail e
    Right (solutionA, solutionB) -> do
      when (PartA `elem` parts) $ putStrLn $ "Part A: " <> solutionA
      when (PartB `elem` parts) $ putStrLn $ "Part B: " <> solutionB

solutionsFor :: Day -> Solution
solutionsFor day = case day of
  1 -> solutions Day01.parse Day01.solveA Day01.solveB
  2 -> solutions Day02.parse Day02.solveA Day02.solveB
  3 -> solutions Day03.parse Day03.solveA Day03.solveB
  _ -> error $ "Have not solved for Day " <> show day <> " yet"

solutions :: Show a => (String -> Either ParseError r) -> (r -> a) -> (r -> a) -> Solution
solutions parse solveA solveB input = do
  parsed <- parse input
  pure $ (show . ($ parsed)) `both` (solveA, solveB)

cli :: Opt.ParserInfo Options
cli =
  Opt.info (Opt.helper <*> opts) $
    Opt.fullDesc
    <> Opt.header "Solutions to Advent of Code 2021"
    <> Opt.progDesc "Run solution(s) for the AoC puzzle of the given day"

opts :: Opt.Parser Options
opts =
  Options
    <$> (Opt.option (dayNumberOpt 25) $ short 'd' <> long "day" <> metavar "N" <> help "Which day's solution to run")
    <*> (buildDayPart
      <$> Opt.switch (short 'a' <> help "Run only part A of the day's solution")
      <*> Opt.switch (short 'b' <> help "Run only part B of the day's solution"))
    <*> (Key <$> Opt.strOption (long "key" <> short 'k' <> metavar "KEY" <> help "API session key"))

fetchInput :: Options -> IO String
fetchInput Options{ day, key } = do
  response <- Advent.runAoC (aocOpts key) (Advent.AoCInput $ Advent.mkDay_ $ toInteger day)
  case response of
    Left err -> fail $ "Error response from API: " <> show err
    Right input -> pure $ unpack input

dayNumberOpt :: Int -> Opt.ReadM Day
dayNumberOpt bound =
  Opt.eitherReader $ \s -> do
    n <- maybeToEither errorMessage $ readMaybe s
    if n < 1 || n > bound
      then Left errorMessage
      else Right n
  where
    errorMessage = "There are " <> show bound <> " days of Christmas. Please specify one of them."
    
buildDayPart :: Bool -> Bool -> [DayPart]
buildDayPart False False = [PartA, PartB]
buildDayPart a b = catMaybes [maybeIf a PartA, maybeIf b PartB]

aocOpts :: SessionKey -> Advent.AoCOpts
aocOpts (Key key) =
  defaultOpts { Advent._aCache = Just "inputs" }
  where defaultOpts = Advent.defaultAoCOpts 2021 key
