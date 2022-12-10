-- | Advent of Code 2022 - Solution for day 10

module Day10
    ( solve
    , test
    ) where

import           Data.List.Extra                ( chunksOf
                                                , drop1
                                                , foldl'
                                                , trimEnd
                                                )
import           Data.List.HT                   ( sieve )
import           Text.Read                      ( readMaybe )

parse :: [String] -> [Maybe Int]
parse = map (readMaybe . drop1 . snd . splitAt 3)

apply :: [Int] -> Maybe Int -> [Int]
apply (x : xs) (Just cmd) = x + cmd : x : x : xs
apply (x : xs) Nothing    = x : x : xs
apply []       _          = error "empty input"

sprite :: Int -> [Int]
sprite pos = [pos - 1 .. pos + 1]

draw :: (Int, Int) -> Char
draw (pos, sig) = if pred pos `mod` 40 `elem` sprite sig then '#' else '.'

solve :: [String] -> (Maybe String, Maybe String)
solve xs = (Just result1, Just result2)
  where
    result1 =
        show . sum . map (uncurry (*)) . sieve 40 . drop 19 $ take 220 signal
    result2 = ('\n' :) . trimEnd . unlines . take 6 . chunksOf 40 $ map draw signal
    signal  = zip [1 ..] . reverse . foldl' apply [1] $ parse xs

sample :: [String]
sample = -- 13140
    [ "addx 15"
    , "addx -11"
    , "addx 6"
    , "addx -3"
    , "addx 5"
    , "addx -1"
    , "addx -8"
    , "addx 13"
    , "addx 4"
    , "noop"
    , "addx -1"
    , "addx 5"
    , "addx -1"
    , "addx 5"
    , "addx -1"
    , "addx 5"
    , "addx -1"
    , "addx 5"
    , "addx -1"
    , "addx -35"
    , "addx 1"
    , "addx 24"
    , "addx -19"
    , "addx 1"
    , "addx 16"
    , "addx -11"
    , "noop"
    , "noop"
    , "addx 21"
    , "addx -15"
    , "noop"
    , "noop"
    , "addx -3"
    , "addx 9"
    , "addx 1"
    , "addx -3"
    , "addx 8"
    , "addx 1"
    , "addx 5"
    , "noop"
    , "noop"
    , "noop"
    , "noop"
    , "noop"
    , "addx -36"
    , "noop"
    , "addx 1"
    , "addx 7"
    , "noop"
    , "noop"
    , "noop"
    , "addx 2"
    , "addx 6"
    , "noop"
    , "noop"
    , "noop"
    , "noop"
    , "noop"
    , "addx 1"
    , "noop"
    , "noop"
    , "addx 7"
    , "addx 1"
    , "noop"
    , "addx -13"
    , "addx 13"
    , "addx 7"
    , "noop"
    , "addx 1"
    , "addx -33"
    , "noop"
    , "noop"
    , "noop"
    , "addx 2"
    , "noop"
    , "noop"
    , "noop"
    , "addx 8"
    , "noop"
    , "addx -1"
    , "addx 2"
    , "addx 1"
    , "noop"
    , "addx 17"
    , "addx -9"
    , "addx 1"
    , "addx 1"
    , "addx -3"
    , "addx 11"
    , "noop"
    , "noop"
    , "addx 1"
    , "noop"
    , "addx 1"
    , "noop"
    , "noop"
    , "addx -13"
    , "addx -19"
    , "addx 1"
    , "addx 3"
    , "addx 26"
    , "addx -30"
    , "addx 12"
    , "addx -1"
    , "addx 3"
    , "addx 1"
    , "noop"
    , "noop"
    , "noop"
    , "addx -9"
    , "addx 18"
    , "addx 1"
    , "addx 2"
    , "noop"
    , "noop"
    , "addx 9"
    , "noop"
    , "noop"
    , "noop"
    , "addx -1"
    , "addx 2"
    , "addx -37"
    , "addx 1"
    , "addx 3"
    , "noop"
    , "addx 15"
    , "addx -21"
    , "addx 22"
    , "addx -6"
    , "addx 1"
    , "noop"
    , "addx 2"
    , "addx 1"
    , "noop"
    , "addx -10"
    , "noop"
    , "noop"
    , "addx 20"
    , "addx 1"
    , "addx 2"
    , "addx 2"
    , "addx -6"
    , "addx -11"
    , "noop"
    , "noop"
    , "noop"
    ]

test :: (Maybe String, Maybe String)
test = solve sample
