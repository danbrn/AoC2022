-- | Advent of Code 2022 - Solution for day 14

module Day14
    ( solve
    , test
    ) where

import qualified Data.HashSet                  as S
import           Data.List.Extra                ( find
                                                , foldl'
                                                , splitOn
                                                )
import           Data.Tuple.Extra               ( both )
import           Prelude                 hiding ( floor )

import           Util

data Bottom = Abyss Int | Floor Int

type Position = (Int, Int)
type RockMap = S.HashSet Position
type SandMap = S.HashSet Position

fallDirections :: [Position]
fallDirections = [(0, 1), (-1, 1), (1, 1)]

origin :: Position
origin = (500, 0)

fall :: Bottom -> Position -> RockMap -> SandMap -> (Bool, SandMap)
fall b p rm sm = case b of
    Abyss a -> if snd p >= a then (True, sm) else fall' (const True)
    Floor f -> fall' (\(_, y) -> y < pred f)
  where
    fall' bcheck =
        case
                find
                        (\p' -> bcheck p' && not (S.member p' sm) && not
                            (S.member p' rm)
                        )
                    $ map (addPairs p) fallDirections
            of
                Nothing -> (False, S.insert p sm)
                Just p' -> fall b p' rm sm

dropSand :: Bottom -> RockMap -> SandMap -> SandMap
dropSand b rm sm = if S.member origin sm
    then sm
    else case fall b origin rm sm of
        (True , sm') -> sm'
        (False, sm') -> dropSand b rm sm'

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
    rockMap = foldl' (flip S.insert) S.empty walls
    walls   = concatMap wall xs
    abyss   = 1 + maximum (map snd walls)
    floor   = 2 + abyss
    solve1  = S.size $ dropSand (Abyss abyss) rockMap S.empty
    solve2  = S.size $ dropSand (Floor floor) rockMap S.empty

sample :: [String]
sample = -- a: 24, b: 93
    ["498,4 -> 498,6 -> 496,6", "503,4 -> 502,4 -> 502,9 -> 494,9"]

test :: (Maybe String, Maybe String)
test = solve sample
