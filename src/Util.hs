-- | Advent of Code 2022 - Some utility functions

module Util
    ( input
    , tuplify2
    , tuplify3
    , pair
    , pairUp
    , halve
    , opPairs
    , addPairs
    , subPairs
    , manhattan
    , (.:)
    , (.:.)
    , (.::)
    ) where

import           Data.Bifunctor                 ( bimap )
import           Data.Composition               ( (.:)
                                                , (.:.)
                                                , (.::)
                                                )
import           Data.List.Extra                ( chunksOf )
import           Data.Tuple.Extra               ( both )
import           System.Directory               ( doesFileExist )

-- | Read input (if it exists)
input :: String -> IO [String]
input fn = do
    exists <- doesFileExist fn
    if exists then lines <$> readFile fn else pure []

-- | Create a tuple from a list with two elements
tuplify2 :: [a] -> (a, a)
tuplify2 [x, y] = (x, y)
tuplify2 _      = error "illegal list length"

-- | Create a tuple from a list with three elements
tuplify3 :: [a] -> (a, a, a)
tuplify3 [x, y, z] = (x, y, z)
tuplify3 _         = error "illegal list length"

-- | Alias for tuplify2
pair :: [a] -> (a, a)
pair = tuplify2

-- | Splits list into pairs (and dies if uneven)
pairUp :: [a] -> [(a, a)]
pairUp = map pair . chunksOf 2

-- | Split a list into two of (about) equal size
halve :: [a] -> ([a], [a])
halve xs = splitAt ((length xs + 1) `div` 2) xs

-- | Apply operator to two pairs
opPairs :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
opPairs f x = uncurry bimap (both f x)

-- | Add two pairs
addPairs :: Num a => (a, a) -> (a, a) -> (a, a)
addPairs = opPairs (+)

-- | Subtract pairs
subPairs :: Num a => (a, a) -> (a, a) -> (a, a)
subPairs = opPairs (-)

-- | Calculates the Manhattan distance between two pairs
manhattan :: Num a => (a, a) -> (a, a) -> a
manhattan = uncurry (+) . both abs .: subPairs
