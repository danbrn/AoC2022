{-# LANGUAGE TupleSections #-}

-- | Advent of Code 2022 - Solution for day 08

module Day08
    ( solve
    , test
    ) where

import           Data.Char                      ( digitToInt )
import           Data.List.Extra                ( foldl'
                                                , nubSort
                                                , tails
                                                , transpose
                                                )

visibleRow :: [Int] -> [Int]
visibleRow row = nubSort $ vr row ++ map (len - 1 -) (vr (reverse row))
  where
    vr ts = snd . foldl' isVisible (-1, []) $ zip [0 ..] ts
    isVisible (m, vs) (i, t) = if t > m then (t, i : vs) else (m, vs)
    len = length row

visible :: [[Int]] -> Int
visible trees = length . nubSort $ concat visibleH ++ concat visibleV
  where
    visibleH =
        foldl' (\(y, tss) ts -> (y + 1, tss ++ map (, y) ts)) (0, [])
            $ map visibleRow trees
    visibleV =
        foldl' (\(x, tss) ts -> (x + 1, tss ++ map (x, ) ts)) (0, [])
            . map visibleRow
            $ transpose trees

distanceRow :: [Int] -> [Int]
distanceRow row = zipWith (*) (dr row) (reverse $ dr $ reverse row)
  where
    dr ts' =
        map
                (\ts ->
                    let val = length $ takeWhile (head ts >) (tail ts)
                    in  if not . null . drop val $ tail ts then val + 1 else val
                )
            . filter (not . null)
            $ tails ts'

distance :: [[Int]] -> Int
distance trees = maximum . concat $ zipWith (zipWith (*)) distanceH distanceV
  where
    distanceH = map distanceRow trees
    distanceV = transpose $ map distanceRow $ transpose trees

solve :: [String] -> (Maybe String, Maybe String)
solve xs = (Just $ show $ visible trees, Just $ show $ distance trees)
    where trees = map (map digitToInt) xs

sample :: [String]
sample = -- a: 21, b: 8
    ["30373", "25512", "65332", "33549", "35390"]

test :: (Maybe String, Maybe String)
test = solve sample
