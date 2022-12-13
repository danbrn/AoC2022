-- | Advent of Code 2022 - Solution for day 13

module Day13
    ( solve
    , test
    ) where

import           Data.Char                      ( isDigit )
import           Data.List.Extra                ( chunksOf
                                                , elemIndex
                                                , headDef
                                                , sort
                                                )
import           Data.Maybe                     ( mapMaybe )
import           Data.Void
import           Text.Megaparsec                ( Parsec
                                                , parseMaybe
                                                )
import qualified Text.Megaparsec               as P
import qualified Text.Megaparsec.Char          as P

import           Util

data Tree = Node [Tree] | Leaf Int
  deriving Eq

instance Ord Tree where
    compare = (<=>)

(.<.) :: Tree -> Tree -> Bool
l .<. r = l <=> r == LT

(<=>) :: Tree -> Tree -> Ordering
Leaf l <=> Leaf r = l `compare` r
Node l <=> Leaf r = l .<=>. [Leaf r]
Leaf l <=> Node r = [Leaf l] .<=>. r
Node l <=> Node r = l .<=>. r

(.<=>.) :: [Tree] -> [Tree] -> Ordering
l .<=>. r = headDef (length l `compare` length r) . dropWhile (== EQ) $ zipWith
    (<=>)
    l
    r

type Parser = Parsec Void String

intParser :: Parser Tree
intParser = do
    digits <- P.some (P.satisfy isDigit)
    pure $ Leaf $ read digits

listParser :: Parser Tree
listParser = do
    _     <- P.char '['
    elems <- (listParser P.<|> intParser) `P.sepBy` P.char ','
    _     <- P.char ']'
    pure $ Node elems

solve :: [String] -> (Maybe String, Maybe String)
solve xs = (Just solve1, Just solve2)
  where
    lists    = mapMaybe (parseMaybe listParser) xs
    pairs    = map pair $ chunksOf 2 lists
    dividers = mapMaybe (parseMaybe listParser) ["[[2]]", "[[6]]"]
    sorted   = sort $ lists ++ dividers
    solve1   = show $ sum $ map fst $ filter snd $ zip [1 :: Int ..] $ map
        (uncurry (.<.))
        pairs
    solve2 = show $ product $ map succ $ mapMaybe (`elemIndex` sorted) dividers

sample :: [String]
sample =
    [ "[1,1,3,1,1]"
    , "[1,1,5,1,1]"
    , ""
    , "[[1],[2,3,4]]"
    , "[[1],4]"
    , ""
    , "[9]"
    , "[[8,7,6]]"
    , ""
    , "[[4,4],4,4]"
    , "[[4,4],4,4,4]"
    , ""
    , "[7,7,7,7]"
    , "[7,7,7]"
    , ""
    , "[]"
    , "[3]"
    , ""
    , "[[[]]]"
    , "[[]]"
    , ""
    , "[1,[2,[3,[4,[5,6,7]]]],8,9]"
    , "[1,[2,[3,[4,[5,6,0]]]],8,9]"
    ]

test :: (Maybe String, Maybe String)
test = solve sample
