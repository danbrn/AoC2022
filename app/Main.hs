module Main
  ( main
  ) where

import           Control.Monad                  ( forM_ )
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromMaybe )
import           Options.Applicative            ( Parser
                                                , argument
                                                , execParser
                                                , fullDesc
                                                , header
                                                , help
                                                , helper
                                                , info
                                                , metavar
                                                , str
                                                )
import           Text.Printf                    ( printf )
import           Text.Read                      ( readMaybe )

import           Util                           ( input )

import           Day01
import           Day02
import           Day03
import           Day04
import           Day05
import           Day06
import           Day07
import           Day08
import           Day09
import           Day10
import           Day11
import           Day12
import           Day13
import           Day14
{-- insert import here --}

days :: Map Int ([String] -> (Maybe String, Maybe String))
days = M.fromList
  [ (1, Day01.solve)
  , (2, Day02.solve)
  , (3, Day03.solve)
  , (4, Day04.solve)
  , (5, Day05.solve)
  , (6, Day06.solve)
  , (7, Day07.solve)
  , (8, Day08.solve)
  , (9, Day09.solve)
  , (10, Day10.solve)
  , (11, Day11.solve)
  , (12, Day12.solve)
  , (13, Day13.solve)
  , (14, Day14.solve)
  {-- insert map entry here --}
  ]

newtype Arguments = Arguments
  { dayNumber :: String
  }

arguments :: Parser Arguments
arguments = Arguments <$> argument
  str
  (metavar "DAY" <> help "Number of the day to run, or \"all\" for all days")

day :: Int -> IO ()
day n = do
  let daynum = printf "%02d" n
  let mf = fromMaybe (const (Nothing, Nothing)) (days M.!? n)
  xs <- input $ "data/input" ++ daynum ++ ".txt"
  let (a, b) = mf xs
  putStrLn $ daynum ++ "a: " ++ fromMaybe "-" a
  putStrLn $ daynum ++ "b: " ++ fromMaybe "-" b

runDay :: Arguments -> IO ()
runDay arg = case dayNumber arg of
  "all" -> forM_ [1 .. 25] day
  dn    -> maybe (putStrLn "Illegal day number") day (readMaybe dn)

main :: IO ()
main = execParser args >>= runDay
 where
  args = info
    (helper <*> arguments)
    (fullDesc <> header "AoC2022 - run solutions for Advent of Code 2022")
