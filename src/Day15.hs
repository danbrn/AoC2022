-- | Advent of Code 2022 - Solution for day 15

module Day15
    ( solve
    , test
    ) where

import           Data.Char                      ( isDigit )
import           Data.List.Extra                ( find
                                                , nubOrd
                                                , splitOn
                                                )
import           Data.Maybe                     ( mapMaybe )
import           Data.Range                     ( (+=+)
                                                , Bound(..)
                                                , BoundType(..)
                                                , Range(..)
                                                , difference
                                                , fromRanges
                                                , inRanges
                                                , invert
                                                , lbe
                                                , ube
                                                )
import           Data.Tuple.Extra               ( both
                                                , first
                                                )
import           Util

type Position = (Int, Int)
type SensorBeacon = (Position, Position)

parse :: String -> SensorBeacon
parse =
    both (both read) . pair . map (pair . splitOn ",") . splitOn ":" . filter
        (\x -> isDigit x || x `elem` [',', '-', ':'])

rangesForRow :: Int -> [SensorBeacon] -> [Range Int]
rangesForRow y = mapMaybe rangeForRow
  where
    rangeForRow :: SensorBeacon -> Maybe (Range Int)
    rangeForRow sb@((sx, sy), _) =
        let dist     = uncurry manhattan sb
            dy       = abs $ sy - y
            dx       = dist - dy
            (x1, x2) = (sx - dx, sx + dx)
        in  if x2 < x1 then Nothing else Just (x1 +=+ x2)

rangeSize :: Range Int -> Int
rangeSize (SpanRange (Bound b Inclusive) (Bound e Inclusive)) = e - b + 1
rangeSize (SpanRange (Bound b Exclusive) (Bound e Exclusive)) = e - b - 1
rangeSize (SpanRange (Bound b _) (Bound e _)) = e - b
rangeSize (SingletonRange _) = 1
rangeSize _                  = error "illegal range"

checkRow :: Int -> [SensorBeacon] -> Int
checkRow y sbs = sum $ map rangeSize rs'
  where
    ss  = map (SingletonRange . fst) . filter ((== y) . snd) $ map fst sbs
    bs  = map (SingletonRange . fst) . filter ((== y) . snd) $ map snd sbs
    rs  = rangesForRow y sbs
    rs' = rs `difference` ss `difference` bs

findBeacon :: Int -> [SensorBeacon] -> Position
findBeacon lim sbs = findBeacon' 0
  where
    findBeacon' row
        | row > lim
        = error "not found"
        | otherwise
        = let
              rs =
                  filter (`notElem` [ube 0, lbe lim])
                      $ invert
                            (            rangesForRow row sbs
                            `difference` [ube 0]
                            `difference` [lbe lim]
                            )
              x = fromRanges rs
          in
              if null x then findBeacon' (succ row) else (head x, row)

solve :: [String] -> (Maybe String, Maybe String)
solve xs =
    ( Just $ show $ checkRow row sbs
    , Just $ show $ uncurry (+) $ first (* 4000000) $ findBeacon lim sbs
    )
  where
    sbs  = map parse xs
    maxY = maximum (map (snd . fst) sbs ++ map (snd . snd) sbs)
    row  = if maxY < 100 then 10 else 2000000
    lim  = if maxY < 100 then 20 else 4000000

sample :: [String]
sample = -- a: y=10 => 26
    [ "Sensor at x=2, y=18: closest beacon is at x=-2, y=15"
    , "Sensor at x=9, y=16: closest beacon is at x=10, y=16"
    , "Sensor at x=13, y=2: closest beacon is at x=15, y=3"
    , "Sensor at x=12, y=14: closest beacon is at x=10, y=16"
    , "Sensor at x=10, y=20: closest beacon is at x=10, y=16"
    , "Sensor at x=14, y=17: closest beacon is at x=10, y=16"
    , "Sensor at x=8, y=7: closest beacon is at x=2, y=10"
    , "Sensor at x=2, y=0: closest beacon is at x=2, y=10"
    , "Sensor at x=0, y=11: closest beacon is at x=2, y=10"
    , "Sensor at x=20, y=14: closest beacon is at x=25, y=17"
    , "Sensor at x=17, y=20: closest beacon is at x=21, y=22"
    , "Sensor at x=16, y=7: closest beacon is at x=15, y=3"
    , "Sensor at x=14, y=3: closest beacon is at x=15, y=3"
    , "Sensor at x=20, y=1: closest beacon is at x=15, y=3"
    ]

test :: (Maybe String, Maybe String)
test = solve sample
