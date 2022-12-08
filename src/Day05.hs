-- | Advent of Code 2022 - Solution for day 05

module Day05
  ( solve
  , test
  ) where

import           Data.Bifunctor                 ( bimap )
import           Data.Char                      ( isAlpha )
import           Data.List                      ( foldl'
                                                , transpose
                                                )
import           Data.List.HT                   ( sieve )
import           Data.List.Split                ( splitOn )

import           Util                           ( pair
                                                , tuplify3
                                                )

parseStacks :: [String] -> [String]
parseStacks = map (filter isAlpha) . transpose . map (sieve 4 . tail)

parseMoves :: [String] -> [(Int, Int, Int)]
parseMoves = map (tuplify3 . map read . sieve 2 . tail . words)

move :: (Int, Int, Int) -> [String] -> [String]
move (0, _   , _ ) stacks = stacks
move (n, from, to) stacks = move (n - 1, from, to)
  $ zipWith update [1 ..] stacks
 where
  stack             = stacks !! (from - 1)
  (crate, oldstack) = (head stack, tail stack)
  newstack          = crate : stacks !! (to - 1)
  update i cs | i == from = oldstack
              | i == to   = newstack
              | otherwise = cs

move9001 :: (Int, Int, Int) -> [String] -> [String]
move9001 (n, from, to) stacks = zipWith update [1 ..] stacks
 where
  stack              = stacks !! (from - 1)
  (crates, oldstack) = splitAt n stack
  newstack           = crates ++ stacks !! (to - 1)
  update i cs | i == from = oldstack
              | i == to   = newstack
              | otherwise = cs

solve :: [String] -> (Maybe String, Maybe String)
solve xs = (Just $ map head newstacks, Just $ map head newstacks9001)
 where
  (stacks, moves) = bimap parseStacks parseMoves $ pair $ splitOn [[]] xs
  newstacks       = foldl' (flip move) stacks moves
  newstacks9001   = foldl' (flip move9001) stacks moves

sample :: [String]
sample =
  [ "    [D]    "
  , "[N] [C]    "
  , "[Z] [M] [P]"
  , " 1   2   3 "
  , ""
  , "move 1 from 2 to 1"
  , "move 3 from 1 to 3"
  , "move 2 from 2 to 1"
  , "move 1 from 1 to 2"
  ]

test :: (Maybe String, Maybe String)
test = solve sample
