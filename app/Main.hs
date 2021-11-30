module Main where
    
import Data.Either.Extra (maybeToEither)
import Data.Maybe (catMaybes)
import Options.Applicative (help, long, metavar, short)
import qualified Options.Applicative as Opt
import Text.Read (readMaybe)
import Lib.Utils (maybeIf)

data Options = Options
  { day :: Int
  , parts :: [DayPart]
  }
  deriving (Show)

data DayPart = PartA | PartB
  deriving (Show, Read)

main :: IO ()
main = do
  options <- Opt.execParser cli
  print options

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

dayNumberOpt :: Int -> Opt.ReadM Int
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