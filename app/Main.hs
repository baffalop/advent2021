{-# LANGUAGE NamedFieldPuns, LambdaCase #-}

module Main where
    
import Control.Monad (when)
import Data.Either.Extra (maybeToEither)
import Data.Maybe (catMaybes)
import Options.Applicative (help, long, metavar, short)
import System.Directory.Extra (doesFileExist)
import Text.Read (readMaybe)
import qualified Options.Applicative as Opt

import Lib.Utils (maybeIf)
import qualified Day1.Solution as Day1

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

data Solution r a = Solution
  { parse :: String -> r
  , solveA :: r -> a
  , solveB :: r -> a
  }

main :: IO ()
main = do
  options@Options{ day, parts } <- Opt.execParser cli

  input <- fetchInput options
  when (length input < 20) $
    error "Input is suspiciously small. Are you sure you piped the right thing?"

  let Solution{ parse, solveA, solveB } = case day of
        1 -> Solution Day1.parse Day1.solveA Day1.solveB
        _ -> error $ "Have not solved for Day " <> show day <> " yet"

  let parsed = parse input
  when (PartA `elem` parts) $ putStrLn $ "Part A: " <> show (solveA parsed)
  when (PartB `elem` parts) $ putStrLn $ "Part B: " <> show (solveB parsed)

-- solutionForDay :: Show a => Int -> Solution r a
-- solutionForDay n = case n of
--   1 -> Solution Day1.parse Day1.solveA Day1.solveB
--   _ -> error $ "Have not solved for Day " <> show n <> " yet"

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
  let inputFilePath = inputPathFor day
  inputFileExists <- doesFileExist inputFilePath
  if inputFileExists
    then do
      putStrLn $ "Reading input from file: " <> inputFilePath
      readFile inputFilePath
    else do
      putStrLn $ "File does not exist: " <> inputFilePath
      putStrLn "Fetching from API"
      fetchInputFromApi day key

inputPathFor :: Day -> FilePath
inputPathFor day = "inputs/Day" <> show day <> ".txt"

fetchInputFromApi :: Day -> SessionKey -> IO String
fetchInputFromApi day key = error "not implemented"

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