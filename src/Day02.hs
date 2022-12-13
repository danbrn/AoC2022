-- | Advent of Code 2022 - Solution for day 02

module Day02
    ( solve
    , test
    ) where

import           Util

import           Data.Bifunctor                 ( bimap )
import           Data.List.Split                ( splitOn )

data Shape = Rock | Paper | Scissors
  deriving (Show, Eq)

data Result = Win | Tie | Loss
  deriving (Show, Eq)

losesTo :: Shape -> Shape
losesTo Rock     = Scissors
losesTo Paper    = Rock
losesTo Scissors = Paper

winsOver :: Shape -> Shape
winsOver Rock     = Paper
winsOver Paper    = Scissors
winsOver Scissors = Rock

shape :: String -> Shape
shape "A" = Rock
shape "B" = Paper
shape "C" = Scissors
shape "X" = Rock
shape "Y" = Paper
shape "Z" = Scissors
shape _   = error "illegal shape"

wantedResult :: String -> Result
wantedResult "X" = Loss
wantedResult "Y" = Tie
wantedResult "Z" = Win
wantedResult _   = error "illegal result"

result :: Shape -> Shape -> Result
result x y | y == winsOver x = Win
           | y == losesTo x  = Loss
           | otherwise       = Tie

shapeScore :: Shape -> Int
shapeScore Rock     = 1
shapeScore Paper    = 2
shapeScore Scissors = 3

resultScore :: Shape -> Shape -> Int
resultScore x y = case result x y of
    Win  -> 6
    Tie  -> 3
    Loss -> 0

matchScore :: Shape -> Shape -> Int
matchScore x y = shapeScore y + resultScore x y

matchScore' :: Shape -> Result -> Int
matchScore' x y = case y of
    Win  -> 6 + shapeScore (winsOver x)
    Tie  -> 3 + shapeScore x
    Loss -> shapeScore (losesTo x)

solve :: [String] -> (Maybe String, Maybe String)
solve xs = (Just $ show score, Just $ show score')
  where
    pairs  = map (pair . splitOn " ") xs
    score  = sum $ map (uncurry matchScore . bimap shape shape) pairs
    score' = sum $ map (uncurry matchScore' . bimap shape wantedResult) pairs

sample :: [String]
sample = -- a: 15. b: 12
    ["A Y", "B X", "C Z"]

test :: (Maybe String, Maybe String)
test = solve sample
