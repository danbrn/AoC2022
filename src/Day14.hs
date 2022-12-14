-- | Advent of Code 2022 - Solution for day 14

module Day14
    ( solve
    , test
    ) where

import           Data.List.Extra                ( find
                                                , foldl'
                                                , splitOn
                                                )
import qualified Data.Map                      as M
import           Data.Maybe                     ( isJust
                                                , isNothing
                                                )
import           Data.Tuple.Extra               ( both )
import           Prelude                 hiding ( floor )

import           Util

data Tile = Rock | Sand
  deriving (Show, Eq)

data Bottom = Abyss Int | Floor Int

type Position = (Int, Int)
type CaveMap = M.Map Position Tile

fallDirections :: [Position]
fallDirections = [(0, 1), (-1, 1), (1, 1)]

fall :: Bottom -> Position -> CaveMap -> (Bool, CaveMap)
fall b p c = case b of
    Abyss a -> if snd p >= a then (True, c) else fall' (const True)
    Floor f -> fall' (\(_, y) -> y < pred f)
  where
    fall' bcheck =
        case
                find (\p' -> bcheck p' && isNothing (M.lookup p' c))
                    $ map (addPairs p) fallDirections
            of
                Nothing -> (False, M.insert p Sand c)
                Just p' -> fall b p' c


dropSand :: Bottom -> CaveMap -> CaveMap
dropSand b c = if isJust $ M.lookup (500, 0) c
    then c
    else case fall b (500, 0) c of
        (True , c') -> c'
        (False, c') -> dropSand b c'

wall :: String -> [Position]
wall xs = concat $ zipWith wall' corners (tail corners)
  where
    corners = map (both read . pair . splitOn ",") $ splitOn " -> " xs
    wall' p1@(x1, y1) p2@(x2, y2) = case opPairs compare p1 p2 of
        (EQ, _) -> [ (x1, y) | y <- [min y1 y2 .. max y1 y2] ]
        _       -> [ (x, y1) | x <- [min x1 x2 .. max x1 x2] ]

solve :: [String] -> (Maybe String, Maybe String)
solve xs = (Just $ show solve1, Just $ show solve2)
  where
    sand   = length . M.filter (== Sand)
    cave   = foldl' (\c p -> M.insert p Rock c) M.empty walls
    walls  = concatMap wall xs
    abyss  = 1 + maximum (map snd walls)
    floor  = 2 + abyss
    solve1 = sand $ dropSand (Abyss abyss) cave
    solve2 = sand $ dropSand (Floor floor) cave

sample :: [String]
sample = -- a: 24, b: 93
    ["498,4 -> 498,6 -> 496,6", "503,4 -> 502,4 -> 502,9 -> 494,9"]

test :: (Maybe String, Maybe String)
test = solve sample
