{-# LANGUAGE NamedFieldPuns, LambdaCase #-}

module Main where
    
import Control.Monad (when)
import Data.Either.Extra (maybeToEither)
import Data.Maybe (catMaybes)
import Options.Applicative (help, long, metavar, short)
import Text.Read (readMaybe)
import qualified Advent
import qualified Options.Applicative as Opt
import Data.Text (Text)

import Lib.Utils (maybeIf)
import qualified Day01.Solution as Day01
import qualified Day02.Solution as Day02
import qualified Day03.Solution as Day03
import qualified Day04.Solution as Day04
import qualified Day05.Solution as Day05
import qualified Day06.Solution as Day06
import qualified Day07.Solution as Day07
import qualified Day08.Solution as Day08
import qualified Day10.Solution as Day10
import qualified Day12.Solution as Day12
import qualified Day13.Solution as Day13

data Options = Options
  { day :: Day
  , parts :: [DayPart]
  , submit :: Bool
  , key :: SessionKey
  }

type Day = Int

data DayPart = PartA | PartB
  deriving (Eq, Show)

newtype SessionKey = Key String

type Solution = Text -> Either ParseError (String, String)
type ParseError = String

main :: IO ()
main = do
  options@Options{ day, parts, submit } <- Opt.execParser cli
  
  input <- fetchInput options
  case solutionsFor day input of
    Left e -> fail e
    Right (solutionA, solutionB) -> do
      when (PartA `elem` parts) $ putStrLn $ "Part A: " <> solutionA
      when (PartB `elem` parts) $ putStrLn $ "Part B: " <> solutionB

      when submit $ do
        let (part, solution) = case parts of
              [PartA] -> (PartA, solutionA)
              [PartB] -> (PartB, solutionB)
              _ -> error "Please specify part -a or -b to submit"
              
        when (null solution) $ fail "Solution is empty. Will not submit."
        putStrLn "Submitting..."
        submitSolution solution part options

solutionsFor :: Day -> Solution
solutionsFor day = case day of
  1 -> solutions Day01.parse Day01.solveA Day01.solveB
  2 -> solutions Day02.parse Day02.solveA Day02.solveB
  3 -> solutions Day03.parse Day03.solveA Day03.solveB
  4 -> solutions Day04.parse Day04.solveA Day04.solveB
  5 -> solutions Day05.parse Day05.solveA Day05.solveB
  6 -> solutions Day06.parse Day06.solveA Day06.solveB
  7 -> solutions Day07.parse Day07.solveA Day07.solveB
  8 -> solutions Day08.parse Day08.solveA Day08.solveB
  10 -> solutions Day10.parse Day10.solveA Day10.solveB
  12 -> solutions Day12.parse Day12.solveA Day12.solveB
  13 -> solutions Day13.parse Day13.solveA Day13.solveB
  _ -> error $ "Have not solved for Day " <> show day <> " yet"

solutions :: (Show a, Show b) => (Text -> Either ParseError r) -> (r -> a) -> (r -> b) -> Solution
solutions parse solveA solveB input = do
  parsed <- parse input
  pure (show $ solveA parsed, show $ solveB parsed)

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
    <*> Opt.switch (short 's' <> long "submit" <> help "Submit the computed solution (must specify a part)")
    <*> (Key <$> Opt.strOption (long "key" <> short 'k' <> metavar "KEY" <> help "API session key"))

fetchInput :: Options -> IO Text
fetchInput Options{ day, key } = do
  response <- Advent.runAoC (aocOpts key) (Advent.AoCInput $ Advent.mkDay_ $ toInteger day)
  case response of
    Left err -> fail $ "Error response from API: " <> show err
    Right input -> pure input

submitSolution :: String -> DayPart -> Options -> IO ()
submitSolution solution part Options{ day, key } = do
  let apiDay = Advent.mkDay_ $ toInteger day
  let apiPart = case part of
        PartA -> Advent.Part1
        PartB -> Advent.Part2
  
  response <- Advent.runAoC (aocOpts key) $ Advent.AoCSubmit apiDay apiPart solution
  case response of
    Left e -> fail $ "Error submitting: " <> show e
    Right (message, code) -> do
      print message
      putStrLn $ "(Code " <> show code <> ")"

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
  (Advent.defaultAoCOpts 2021 key) { Advent._aCache = Just "." }
