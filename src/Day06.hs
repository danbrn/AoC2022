-- | Advent of Code 2022 - Solution for day 06

module Day06
    ( solve
    , test
    ) where

import           Data.List                      ( find
                                                , nub
                                                , tails
                                                )

solve :: [String] -> (Maybe String, Maybe String)
solve xs = (show . fst <$> pos 4, show . fst <$> pos 14)
  where
    chunks n =
        zip [n :: Int ..]
            . filter ((== n) . length)
            . map (take n)
            . tails
            $ head xs
    pos n = find (\(_, c) -> length (nub c) == n) $ chunks n

sample :: [String]
sample = ["mjqjpqmgbljsphdztnvjfqwrcgsmlb"]

test :: (Maybe String, Maybe String)
test = solve sample
