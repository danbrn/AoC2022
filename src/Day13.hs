-- | Advent of Code 2022 - Solution for day 13

module Day13
    ( solve
    , test
    ) where

import           Data.List.Extra                ( elemIndex
                                                , headDef
                                                , sort
                                                )
import           Data.Maybe                     ( mapMaybe )
import           Data.Void
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
l .<=>. r =
    let tiebreak = length l `compare` length r
    in  headDef tiebreak $ dropWhile (== EQ) $ zipWith (<=>) l r

type Parser = P.Parsec Void String

intParser :: Parser Tree
intParser = Leaf . read <$> P.some P.digitChar

listParser :: Parser Tree
listParser = do
    _     <- P.char '['
    elems <- (listParser P.<|> intParser) `P.sepBy` P.char ','
    _     <- P.char ']'
    pure $ Node elems

solve :: [String] -> (Maybe String, Maybe String)
solve xs = (Just $ show solve1, Just $ show solve2)
  where
    (dividers, lists) =
        splitAt 2 $ mapMaybe (P.parseMaybe listParser) $ "[[2]]" : "[[6]]" : xs
    sorted = sort $ dividers ++ lists
    solve1 =
        sum
            $ map fst
            $ filter snd
            $ zip [1 :: Int ..]
            $ map (uncurry (.<.))
            $ pairUp lists
    solve2 = product $ map succ $ mapMaybe (`elemIndex` sorted) dividers

sample :: [String]
sample = -- a: 13, b: 140
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
