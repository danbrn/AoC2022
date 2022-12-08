-- | Advent of Code 2022 - Solution for day 07

module Day07
    ( solve
    , test
    ) where

import           Data.List                      ( foldl'
                                                , isPrefixOf
                                                , nub
                                                , sort
                                                , sortOn
                                                )
import           Data.List.Split                ( splitOn )
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Ord                       ( Down(Down) )
import           Util                           ( pair )

data Entry = Directory String | File String Int
  deriving (Show, Eq, Ord)

path :: Entry -> String
path (Directory p) = p
path (File p _   ) = p

size :: Entry -> Int
size (Directory _) = 0
size (File _ s   ) = s

isDirectory :: Entry -> Bool
isDirectory (Directory _) = True
isDirectory _             = False

parse :: [String] -> [Entry]
parse = nub . sort . snd . foldl' go ("", [])
  where
    go (pwd, fs) cmd = case cmd of
        "$ cd /"  -> ("", fs)
        "$ cd .." -> (parentPath pwd, fs)
        "$ ls"    -> (pwd, fs)
        _         -> if "$ cd " `isPrefixOf` cmd
            then let p = pwd ++ '/' : drop 5 cmd in (p, Directory p : fs)
            else if "dir" `isPrefixOf` cmd
                then (pwd, fs)
                else
                    let (s, n) = pair $ splitOn " " cmd
                        p      = pwd ++ '/' : n
                    in  (pwd, File p (read s) : fs)

parentPath :: String -> String
parentPath = reverse . tail . dropWhile (/= '/') . reverse

populateMap :: [Entry] -> Map Entry Int
populateMap = foldl' (\m e -> M.insert e (size e) m) M.empty

calculateDirSizes :: Map Entry Int -> Map Entry Int
calculateDirSizes dirs =
    foldl'
            (\a (e, _) ->
                M.insertWith (+) (Directory (parentPath $ path e)) (a M.! e) a
            )
            dirs
        . map snd
        . sortOn (Down . fst)
        . map (\es -> (length . splitOn "/" $ path $ fst es, es))
        $ M.assocs dirs

solve :: [String] -> (Maybe String, Maybe String)
solve xs =
    ( Just $ show $ sum $ filter (<= 100000) dirs
    , Just $ show $ head $ dropWhile (< needed) dirs
    )
  where
    dirs =
        sort
            $ map snd
            $ filter (isDirectory . fst)
            $ M.assocs
            $ calculateDirSizes
            $ populateMap
            $ parse xs
    needed = 30000000 - (70000000 - maximum dirs)


sample :: [String]
sample =
    [ "$ cd /"
    , "$ ls"
    , "dir a"
    , "14848514 b.txt"
    , "8504156 c.dat"
    , "dir d"
    , "$ cd a"
    , "$ ls"
    , "dir e"
    , "29116 f"
    , "2557 g"
    , "62596 h.lst"
    , "$ cd e"
    , "$ ls"
    , "584 i"
    , "$ cd .."
    , "$ cd .."
    , "$ cd d"
    , "$ ls"
    , "4060174 j"
    , "8033020 d.log"
    , "5626152 d.ext"
    , "7214296 k"
    ]

test :: (Maybe String, Maybe String)
test = solve sample
