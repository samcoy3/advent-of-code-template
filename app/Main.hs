module Main where

{- ORMOLU_DISABLE -}
--- Day imports
import qualified Days.Day01 as Day01 (runDay)
import qualified Days.Day02 as Day02 (runDay)
import qualified Days.Day03 as Day03 (runDay)
import qualified Days.Day04 as Day04 (runDay)
import qualified Days.Day05 as Day05 (runDay)
import qualified Days.Day06 as Day06 (runDay)
import qualified Days.Day07 as Day07 (runDay)
import qualified Days.Day08 as Day08 (runDay)
import qualified Days.Day09 as Day09 (runDay)
import qualified Days.Day10 as Day10 (runDay)
import qualified Days.Day11 as Day11 (runDay)
import qualified Days.Day12 as Day12 (runDay)
import qualified Days.Day13 as Day13 (runDay)
import qualified Days.Day14 as Day14 (runDay)
import qualified Days.Day15 as Day15 (runDay)
import qualified Days.Day16 as Day16 (runDay)
import qualified Days.Day17 as Day17 (runDay)
import qualified Days.Day18 as Day18 (runDay)
import qualified Days.Day19 as Day19 (runDay)
import qualified Days.Day20 as Day20 (runDay)
import qualified Days.Day21 as Day21 (runDay)
import qualified Days.Day22 as Day22 (runDay)
import qualified Days.Day23 as Day23 (runDay)
import qualified Days.Day24 as Day24 (runDay)
import qualified Days.Day25 as Day25 (runDay)
{- ORMOLU_ENABLE -}

--- Other imports
import Data.Map (Map)
import qualified Data.Map as Map
import Options.Applicative

data Day = Day (Maybe Int)
  deriving (Show)

validate :: Int -> Maybe Int
validate n = if (n `elem` Map.keys days) then (Just n) else Nothing

dayParser :: Parser Day
dayParser =
  Day
    <$> optional
      ( option
          auto
          ( long "day"
              <> short 'd'
              <> help "Choose a day to print the solutions for. Omitting this option will print out all days."
          )
      )

days :: Map Int (IO ())
days =
  Map.fromList . zip [1 ..] $
    [ Day01.runDay,
      Day02.runDay,
      Day03.runDay,
      Day04.runDay,
      Day05.runDay,
      Day06.runDay,
      Day07.runDay,
      Day08.runDay,
      Day09.runDay,
      Day10.runDay,
      Day11.runDay,
      Day12.runDay,
      Day13.runDay,
      Day14.runDay,
      Day15.runDay,
      Day16.runDay,
      Day17.runDay,
      Day18.runDay,
      Day19.runDay,
      Day20.runDay,
      Day21.runDay,
      Day22.runDay,
      Day23.runDay,
      Day24.runDay,
      Day25.runDay
    ]

performDay :: Day -> IO ()
performDay (Day d) =
  let action = (d >>= validate >>= (days Map.!?))
   in case action of
        Nothing -> putStrLn "Invalid day"
        Just a -> do
          putStrLn "\n********"
          a
          putStrLn "********"

main :: IO ()
main = performDay =<< execParser opts
  where
    opts =
      info
        (dayParser <**> helper)
        (fullDesc <> progDesc "Prints out some Advent of Code solutions.")
