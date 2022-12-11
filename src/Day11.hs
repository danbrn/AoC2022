-- | Advent of Code 2022 - Solution for day 11

module Day11
    ( solve
    , test
    ) where

import           Data.Char                      ( isSpace )
import           Data.List.Extra                ( foldl'
                                                , groupSort
                                                , iterate'
                                                , sortOn
                                                )
import           Data.List.Split                ( splitOn )
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromJust )
import           Data.Ord                       ( Down(..) )
import           Data.Void                      ( Void )
import           Text.Megaparsec                ( Parsec
                                                , many
                                                , some
                                                )
import qualified Text.Megaparsec               as P
import qualified Text.Megaparsec.Char          as P

data Monkey = Monkey
    { num  :: Int
    , itms :: [Int]
    , op   :: Int -> Int
    , dvsr :: Int
    , tst  :: Int -> Int
    , insp :: Int
    }

type Parser = Parsec Void String

type MonkeyMap = Map Int Monkey

monkeyParser :: Parser Monkey
monkeyParser = do
    _    <- P.string "Monkey "
    num  <- integerParser
    _    <- P.char ':'
    _    <- P.newline
    itms <- itemsParser
    _    <- P.newline
    op   <- opParser
    _    <- P.newline
    tst  <- testParser
    let (dvsr, _, _) = tst
    pure $ Monkey num itms op dvsr (mkTst tst) 0

itemsParser :: Parser [Int]
itemsParser = do
    _ <- P.takeWhileP Nothing isSpace
    _ <- P.string "Starting items: "
    many itemParser
  where
    itemParser = do
        _ <- P.takeWhileP Nothing (`elem` [',', ' '])
        integerParser

opParser :: Parser (Int -> Int)
opParser = do
    _  <- P.takeWhileP Nothing isSpace
    _  <- P.string "Operation: new = old "
    op <- many (P.satisfy (/= '\n'))
    pure $ mkOp op

mkOp :: String -> (Int -> Int)
mkOp str = case o of
    '*' -> if n == "old" then (\x -> x * x) else (* read n)
    '+' -> (+ read n)
    x   -> error $ "illegal operator: " ++ [x]
    where (o, n) = (head str, drop 2 str)

testParser :: Parser (Int, Int, Int)
testParser = do
    _         <- P.takeWhileP Nothing isSpace
    _         <- P.string "Test: divisible by "
    divisible <- integerParser
    _         <- P.newline
    ifTrue    <- testResultParser
    _         <- P.newline
    ifFalse   <- testResultParser
    _         <- P.newline
    pure (divisible, ifTrue, ifFalse)

mkTst :: (Int, Int, Int) -> (Int -> Int)
mkTst (d, t, f) x = if x `mod` d == 0 then t else f


testResultParser :: Parser Int
testResultParser = do
    _ <- P.takeWhileP Nothing isSpace
    _ <- P.string "If "
    _ <- P.string "true" P.<|> P.string "false"
    _ <- P.string ": throw to monkey "
    integerParser

integerParser :: Parser Int
integerParser = read <$> P.try (some P.digitChar)

monkeyDescs :: [String] -> [String]
monkeyDescs xs = map unlines $ splitOn [[]] xs

playRound :: Bool -> Int -> MonkeyMap -> MonkeyMap
playRound vw divisor mm = foldl' (flip (monkeyThrow vw divisor)) mm $ M.keys mm

updateThrower :: Monkey -> MonkeyMap -> MonkeyMap
updateThrower m =
    M.insert (num m) (m { itms = [], insp = insp m + length (itms m) })

updateCatchers :: [(Int, [Int])] -> MonkeyMap -> MonkeyMap
updateCatchers items mm = foldl'
    (\mm' (m, items') -> snd $ M.updateLookupWithKey
        (\_ x -> Just $ x { itms = itms x ++ items' })
        m
        mm'
    )
    mm
    items

monkeyThrow :: Bool -> Int -> Int -> MonkeyMap -> MonkeyMap
monkeyThrow vw divisor m mm = updateCatchers addressed
    $ updateThrower monkey mm
  where
    monkey@(Monkey _ items op _ tst insp) = mm M.! m
    inspected x =
        let baseWorry = op x
        in  if vw then baseWorry `mod` divisor else baseWorry `div` 3
    addressed = groupSort $ map ((\x -> (tst x, x)) . inspected) items

solve :: [String] -> (Maybe String, Maybe String)
solve xs = (Just $ solve' False 20, Just $ solve' True 10000)
  where
    solve' vw n =
        show
            $ product
            $ take 2
            $ sortOn Down
            $ map insp
            $ M.elems
            $ (!! n)
            $ iterate' (playRound vw divisor) monkeys
    (divisor, monkeys) =
        (\xs ->
                ( product $ map dvsr xs
                , foldl' (\a x -> M.insert (num x) x a) M.empty xs
                )
            )
            . map (fromJust . P.parseMaybe monkeyParser)
            $ monkeyDescs xs

sample :: [String]
sample =
    [ "Monkey 0:"
    , "  Starting items: 79, 98"
    , "  Operation: new = old * 19"
    , "  Test: divisible by 23"
    , "    If true: throw to monkey 2"
    , "    If false: throw to monkey 3"
    , ""
    , "Monkey 1:"
    , "  Starting items: 54, 65, 75, 74"
    , "  Operation: new = old + 6"
    , "  Test: divisible by 19"
    , "    If true: throw to monkey 2"
    , "    If false: throw to monkey 0"
    , ""
    , "Monkey 2:"
    , "  Starting items: 79, 60, 97"
    , "  Operation: new = old * old"
    , "  Test: divisible by 13"
    , "    If true: throw to monkey 1"
    , "    If false: throw to monkey 3"
    , ""
    , "Monkey 3:"
    , "  Starting items: 74"
    , "  Operation: new = old + 3"
    , "  Test: divisible by 17"
    , "    If true: throw to monkey 0"
    , "    If false: throw to monkey 1"
    ]

test :: (Maybe String, Maybe String)
test = solve sample
