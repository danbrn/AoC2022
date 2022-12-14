{-# LANGUAGE TupleSections #-}

-- | Advent of Code 2022 - Solution for day 12

module Day12
    ( solve
    , test
    ) where

import           Algorithm.Search               ( aStar
                                                , bfs
                                                , dijkstra
                                                )
import           Data.Char                      ( ord )
import           Data.List.Extra                ( foldl' )
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromJust
                                                , mapMaybe
                                                )
import           Data.Tuple.Extra               ( first )

import           Util

type Position = (Int, Int)
type Elevation = Int

type NodeMap = Map Position Node
data Special = Normal | Start | End
    deriving (Show, Eq)

data Node = Node
    { position  :: Position
    , elevation :: Elevation
    , special   :: Special
    , reachable :: [Position]
    }
    deriving Show

pass1 :: [(Position, Char)] -> NodeMap
pass1 = foldl' populate M.empty
  where
    populate m (p, c) = M.insert p node m
      where
        n e s =
            Node { position = p, elevation = e, special = s, reachable = [] }
        node = case c of
            'S' -> n 0 Start
            'E' -> n 25 End
            _   -> n (ord c - ord 'a') Normal

pass2 :: NodeMap -> NodeMap
pass2 m = foldl' updateReachable m $ M.elems m
  where
    adjacent (x, y) = [(x, y + 1), (x + 1, y), (x, y - 1), (x - 1, y)]
    findReachable n =
        map position . filter (\x -> elevation x <= elevation n + 1) $ mapMaybe
            (m M.!?)
            (adjacent $ position n)
    updateReachable m' n =
        M.alter (\_ -> Just $ n { reachable = findReachable n }) (position n) m'

solve :: [String] -> (Maybe String, Maybe String)
solve xs = (Just solve1, Just solve2)
  where
    target = position $ head $ filter ((== End) . special) $ M.elems graph
    solve1 = show . fst . fromJust $ findPathFrom
        (position $ head $ filter ((== Start) . special) $ M.elems graph)
    solve2 =
        show
            $ minimum
            $ map fst
            $ mapMaybe (findPathFrom . position)
            $ filter ((== 0) . elevation)
            $ M.elems graph
    findPathFrom =
        (((,) =<< length) <$>) . bfs (\p -> reachable $ graph M.! p) (== target)
    -- findPathFrom = dijkstra (\p -> reachable $ graph M.! p)
    --                         (\_ _ -> 1 :: Int)
    --                         (== target)
    -- findPathFrom = aStar (\p -> reachable $ graph M.! p)
    --                      (\_ _ -> 1 :: Int)
    --                      (manhattan target)
    --                      (== target)
    graph =
        pass2
            $ pass1
            $ concat
            $ foldl'
                  (\(n, rows) row -> (succ n, rows ++ map (first (, n)) row))
                  (0 :: Int, [])
            $ map (zip [0 :: Int ..]) xs

sample :: [String]
sample = -- a: 31, b: 29
    ["Sabqponm", "abcryxxl", "accszExk", "acctuvwj", "abdefghi"]

test :: (Maybe String, Maybe String)
test = solve sample
