-- | Advent of Code 2022 - Solution for day 12

module Day12
    ( solve
    , test
    ) where

import           Algorithm.Search               ( dijkstra )
import           Data.Char                      ( ord )
import           Data.List.Extra                ( foldl' )
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromJust
                                                , mapMaybe
                                                )
import           Data.Tuple.Extra               ( first )

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

populate :: NodeMap -> (Position, Char) -> NodeMap
populate m (p, c) = M.insert p node m
  where
    n e s = Node { position = p, elevation = e, special = s, reachable = [] }
    node = case c of
        'S' -> n 0 Start
        'E' -> n 26 End
        _   -> n (ord c - ord 'a') Normal

adjacent :: Position -> [Position]
adjacent (x, y) = filter (\(x', y') -> x' >= 0 && y' >= 0)
                         [(x, y + 1), (x + 1, y), (x, y - 1), (x - 1, y)]

findReachable :: NodeMap -> Node -> [Position]
findReachable m n =
    map position . filter (\x -> elevation x <= elevation n + 1) $ mapMaybe
        (m M.!?)
        (adjacent $ position n)

updateReachable :: NodeMap -> Node -> NodeMap
updateReachable m n =
    M.alter (\_ -> Just $ n { reachable = findReachable m n }) (position n) m

pass2 :: NodeMap -> NodeMap
pass2 m = foldl' updateReachable m $ M.elems m

solve :: [String] -> (Maybe String, Maybe String)
solve xs = (Just solve1, Just solve2)
  where
    solve1 = show . fst . fromJust $ findPathFrom
        (position $ head $ filter ((== Start) . special) $ M.elems graph)
    solve2 =
        show
            $ minimum
            $ map fst
            $ mapMaybe (findPathFrom . position)
            $ filter ((== 0) . elevation)
            $ M.elems graph
    findPathFrom = dijkstra (\p -> reachable $ graph M.! p)
                                (\_ _ -> 1 :: Int)
                                (\p -> special (graph M.! p) == End)
    graph =
        pass2
            $ pass1
            $ concat
            $ foldl'
                  (\(n, rows) row -> (succ n, rows ++ map (first (, n)) row))
                  (0 :: Int, [])
            $ map (zip [0 :: Int ..]) xs

sample :: [String]
sample = ["Sabqponm", "abcryxxl", "accszExk", "acctuvwj", "abdefghi"]

test :: (Maybe String, Maybe String)
test = solve sample
