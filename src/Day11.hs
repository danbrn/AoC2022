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
                                                , splitOn
                                                )
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromJust )
import           Data.Ord                       ( Down(..) )
import           Data.Void                      ( Void )
import           Text.Megaparsec                ( (<|>)
                                                , Parsec
                                                , many
                                                , parseMaybe
                                                , satisfy
                                                , sepBy
                                                , some
                                                )
import           Text.Megaparsec.Char           ( char
                                                , digitChar
                                                , newline
                                                , string
                                                )

data Monkey = Monkey
    { num  :: Int
    , itms :: [Int]
    , _op  :: Int -> Int
    , dvsr :: Int
    , _tst :: Int -> Int
    , insp :: Int
    }

type Parser = Parsec Void String

type MonkeyMap = Map Int Monkey

monkeyParser :: Parser Monkey
monkeyParser = do
    _ <- string "Monkey "
    n <- integerParser
    _ <- char ':'
    _ <- newline
    i <- itemsParser
    _ <- newline
    o <- opParser
    _ <- newline
    t <- testParser
    let (d, _, _) = t
    pure $ Monkey n i o d (mkTst t) 0

itemsParser :: Parser [Int]
itemsParser = do
    _ <- many (satisfy isSpace)
    _ <- string "Starting items: "
    integerParser `sepBy` string ", "

opParser :: Parser (Int -> Int)
opParser = do
    _  <- many (satisfy isSpace)
    _  <- string "Operation: new = old "
    op <- many (satisfy (/= '\n'))
    pure $ mkOp op

mkOp :: String -> (Int -> Int)
mkOp str = case o of
    '*' -> if n == "old" then (\x -> x * x) else (* read n)
    '+' -> (+ read n)
    x   -> error $ "illegal operator: " ++ [x]
    where (o, n) = (head str, drop 2 str)

testParser :: Parser (Int, Int, Int)
testParser = do
    _         <- many (satisfy isSpace)
    _         <- string "Test: divisible by "
    divisible <- integerParser
    _         <- newline
    ifTrue    <- testResultParser
    _         <- newline
    ifFalse   <- testResultParser
    _         <- newline
    pure (divisible, ifTrue, ifFalse)

mkTst :: (Int, Int, Int) -> (Int -> Int)
mkTst (d, t, f) x = if x `mod` d == 0 then t else f

testResultParser :: Parser Int
testResultParser = do
    _ <- many (satisfy isSpace)
    _ <- string "If "
    _ <- string "true" <|> string "false"
    _ <- string ": throw to monkey "
    integerParser

integerParser :: Parser Int
integerParser = read <$> some digitChar

monkeyDescs :: [String] -> [String]
monkeyDescs xs = map unlines $ splitOn [[]] xs

playRound :: (Int -> Int) -> MonkeyMap -> MonkeyMap
playRound wm mm = foldl' (flip (monkeyThrow wm)) mm $ M.keys mm

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

monkeyThrow :: (Int -> Int) -> Int -> MonkeyMap -> MonkeyMap
monkeyThrow wm m mm = updateCatchers addressed $ updateThrower monkey mm
  where
    monkey@(Monkey _ items op _ tst _) = mm M.! m
    inspected x = wm $ op x
    addressed = groupSort $ map ((\x -> (tst x, x)) . inspected) items

solve :: [String] -> (Maybe String, Maybe String)
solve xs = (Just $ solve' 20, Just $ solve' 10000)
  where
    solve' n =
        let worryManager = if n < 100 then (`div` 3) else (`mod` divisor)
        in  show
                $ product
                $ take 2
                $ sortOn Down
                $ map insp
                $ M.elems
                $ (!! n)
                $ iterate' (playRound worryManager) monkeys
    (divisor, monkeys) =
        (\xs' ->
                ( product $ map dvsr xs'
                , foldl' (\a x -> M.insert (num x) x a) M.empty xs'
                )
            )
            . map (fromJust . parseMaybe monkeyParser)
            $ monkeyDescs xs

sample :: [String]
sample = -- a: 10605, b: 2713310158
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
