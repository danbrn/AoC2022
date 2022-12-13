-- | Advent of Code 2022 - Solution for day 09

module Day09
    ( solve
    , test
    ) where

import           Control.Monad.State
import           Data.Bifunctor                 ( bimap )
import           Data.List.Extra                ( foldl'
                                                , splitOn
                                                )
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Data.Tuple.Extra               ( both )

import           Util

type Pos = (Int, Int)

data RopeState = RopeState
    { ropePos :: [Pos]
    , visited :: Set Pos
    }

type St = State RopeState

dirDelta :: Char -> Pos
dirDelta 'U' = (0, 1)
dirDelta 'R' = (1, 0)
dirDelta 'D' = (0, -1)
dirDelta 'L' = (-1, 0)
dirDelta x   = error $ "illegal move: " ++ [x]

adjacent :: Pos -> Pos -> Bool
adjacent a b = uncurry (&&) . both ((<= 1) . abs) $ subPairs a b

follow :: Pos -> Pos -> Pos
follow hp tp | adjacent hp tp = tp
             | otherwise      = addPairs tp (delta hp tp)
    where delta h t = both signum $ subPairs h t

moveHead :: Char -> St ()
moveHead d = do
    st <- get
    let rp = ropePos st
    put $ st { ropePos = addPairs (head rp) (dirDelta d) : tail rp }

moveTail :: St ()
moveTail = do
    st <- get
    let rp = ropePos st
    let newrp = reverse
            $ foldl' (\a p -> follow (head a) p : a) [head rp] (tail rp)
    put $ st { ropePos = newrp, visited = last newrp `S.insert` visited st }

move :: [Char] -> St (Set Pos)
move []       = gets visited
move (m : ms) = do
    moveHead m
    moveTail
    move ms

initState :: Int -> RopeState
initState knots =
    RopeState { ropePos = replicate knots (0, 0), visited = S.empty }

solve :: [String] -> (Maybe String, Maybe String)
solve xs = (Just $ solve' 2, Just $ solve' 10)
  where
    moves :: [Char]
    moves = concatMap
        (uncurry (flip replicate) . bimap head read . pair . splitOn " ")
        xs
    solve' l = show $ S.size $ evalState (move moves) (initState l)

sample :: [String]
-- sample = -- a: 13, b: ?
--     ["R 4", "U 4", "L 3", "D 1", "R 4", "D 1", "L 5", "R 2"]
sample = -- a: ?, b: 36
    ["R 5", "U 8", "L 8", "D 3", "R 17", "D 10", "L 25", "U 20"]

test :: (Maybe String, Maybe String)
test = solve sample
