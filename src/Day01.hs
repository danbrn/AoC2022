-- | Advent of Code 2022 - Solution for day 01

module Day01
    ( solve
    , test
    ) where

import           Data.List                      ( sortBy )
import           Data.List.Split                ( splitOn )

solve :: [String] -> (Maybe String, Maybe String)
solve xs = (Just $ show $ head kcals, Just $ show $ sum $ take 3 kcals)
  where
    kcals :: [Int]
    kcals = sortBy (flip compare) $ map (sum . map read) $ splitOn [[]] xs

sample :: [String]
sample =
    [ "1000"
    , "2000"
    , "3000"
    , ""
    , "4000"
    , ""
    , "5000"
    , "6000"
    , ""
    , "7000"
    , "8000"
    , "9000"
    , ""
    , "10000"
    ]

test :: (Maybe String, Maybe String)
test = solve sample
