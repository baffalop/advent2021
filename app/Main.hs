{-# LANGUAGE NamedFieldPuns, LambdaCase #-}

module Main where
    
import Control.Monad (when)
import Data.Either.Extra (maybeToEither)
import Data.Maybe (catMaybes)
import Data.Tuple.Extra (both)
import Options.Applicative (help, long, metavar, short)
import System.Directory.Extra (doesFileExist)
import Text.Read (readMaybe)
import Text.Printf (printf)
import qualified Options.Applicative as Opt

import Lib.Utils (maybeIf)
import qualified Day01.Solution as Day01
import qualified Day02.Solution as Day02

data Options = Options
  { day :: Day
  , parts :: [DayPart]
  , key :: Maybe SessionKey
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
    <*> (fmap Key <$> Opt.optional (Opt.strOption (long "key" <> short 'k' <> metavar "KEY" <> help "API session key")))

fetchInput :: Options -> IO String
fetchInput Options{ day, key } = do
  let inputFilePath = "inputs/Day" <> printf "%02d" day <> ".txt"
  inputFileExists <- doesFileExist inputFilePath
  if inputFileExists
    then do
      putStrLn $ "Reading input from file: " <> inputFilePath
      readFile inputFilePath
    else do
      putStrLn $ "File does not exist: " <> inputFilePath
      putStrLn "Fetching from API"
      case key of
        Just k -> fetchInputFromApi day k
        Nothing -> fail "Please provide a session key to fetch data"

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