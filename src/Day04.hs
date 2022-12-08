-- | Advent of Code 2022 - Solution for day 04

module Day04
    ( solve
    , test
    ) where

import           Data.List.Split                ( splitOn )

import           Util                           ( pair )

parse :: String -> ((Int, Int), (Int, Int))
parse = pair . map (pair . map read . splitOn "-") . splitOn ","

lazy :: (Int, Int) -> (Int, Int) -> Bool
lazy (a1, b1) (a2, b2) = (a1 >= a2 && b1 <= b2) || (a2 >= a1 && b2 <= b1)

overlap :: (Int, Int) -> (Int, Int) -> Bool
overlap (a1, b1) (a2, b2) =
    (a1 >= a2 && a1 <= b2) || (b1 >= a2 && b1 <= b2) || (a1 <= a2 && b1 >= b2)

solve :: [String] -> (Maybe String, Maybe String)
solve xs =
    ( Just $ show $ result $ uncurry lazy
    , Just $ show $ result $ uncurry overlap
    )
    where result f = length $ filter f $ map parse xs

sample :: [String]
sample = ["2-4,6-8", "2-3,4-5", "5-7,7-9", "2-8,3-7", "6-6,4-6", "2-6,4-8"]

test :: (Maybe String, Maybe String)
test = solve sample
