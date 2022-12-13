-- | Advent of Code 2022 - Solution for day 03

module Day03
    ( solve
    , test
    ) where

import           Data.Char                      ( isAlpha
                                                , isAsciiLower
                                                , ord
                                                )
import           Data.List.Extra                ( nubOrd )
import           Data.List.Split                ( chunksOf )

import           Util                           ( halve
                                                , tuplify3
                                                )

priority :: Char -> Int
priority x | not $ isAlpha x = error "illegal contents"
           | isAsciiLower x  = ord x - ord 'a' + 1
           | otherwise       = ord x - ord 'A' + 27

solve :: [String] -> (Maybe String, Maybe String)
solve xs = (Just $ show priorities, Just $ show badgePrios)
  where
    findError (as, bs) = nubOrd $ filter (`elem` bs) as
    priorities = sum $ map priority $ concatMap (findError . halve) xs
    badge (as, bs, cs) = head $ filter (\x -> x `elem` bs && x `elem` cs) as
    badgePrios = sum $ map (priority . badge . tuplify3) $ chunksOf 3 xs

sample :: [String]
sample = -- a: 157, b: 70
    [ "vJrwpWtwJgWrhcsFMMfFFhFp"
    , "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
    , "PmmdzqPrVvPwwTWBwg"
    , "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
    , "ttgJtRGJQctTZtZT"
    , "CrZsJsPPZsGzwwsLwLmpwMDw"
    ]

test :: (Maybe String, Maybe String)
test = solve sample
