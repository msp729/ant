{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE Strict #-}

module Main (main) where

import Control.Monad.State.Strict
import Data.Function
import Data.List
import Data.Ord
import Text.Printf

data Value = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
    deriving (Eq, Enum)

instance Show Value where
    show Ace = "A"
    show Two = "2"
    show Three = "3"
    show Four = "4"
    show Five = "5"
    show Six = "6"
    show Seven = "7"
    show Eight = "8"
    show Nine = "9"
    show Ten = "10"
    show Jack = "J"
    show Queen = "Q"
    show King = "K"

data Suit = Club | Spade | Heart | Diamond deriving (Eq, Enum)
instance Show Suit where
    show Club = "♧"
    show Spade = "♤"
    show Heart = "♡"
    show Diamond = "♢"

data Card = Card {value :: Value, suit :: Suit} deriving (Eq)
instance Show Card where show (Card v s) = show v ++ show s

type Deck = [Card]
type Hand = [Card]
type P x = x -> Bool
type HAND = P Hand

face :: Value -> Bool
face King = True
face Queen = True
face Jack = True
face _ = False

isface :: Card -> Bool
isface = face . value

isheart :: Card -> Bool
isheart = (== Heart) . suit

deck :: Deck
deck = do
    value <- [Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King]
    suit <- [Club, Spade, Heart, Diamond]
    return $ Card value suit

nchands :: Int -> Deck -> [Hand]
nchands n d | n <= 0 = []
nchands 1 d = (: []) <$> d
nchands n (x : xs) = ((x :) <$> nchands (n - 1) xs) ++ nchands n xs
nchands n [] = []

dups :: (Eq a) => [a] -> [a]
dups (x : xs) = if elem x xs then x : dups xs else dups xs
dups [] = []

pair, twopair, three :: HAND
pair h = (== 1) $ length $ dups $ value <$> h
twopair (fmap value -> h) = case dups h of
    [x, y] -> x /= y
    _ -> False
three (fmap value -> h) = case dups h of
    [x, y] -> x == y
    _ -> False

straight :: HAND
straight h = maybe False helper $ uncons $ value <$> sortBy (comparing (fromEnum . value)) h
  where
    helper :: (Value, [Value]) -> Bool
    helper (Ace, rest) = rest == [Two .. Five] || rest == [Ten .. King]
    helper (fromEnum -> x, rest) = fmap fromEnum rest == [x + 1 .. x + 4]

flush :: HAND
flush = (>= 4) . length . dups . fmap suit

fullhouse, four :: HAND
fullhouse h =
    let l = dups $ value <$> h
     in length l == 3
            && length (dups l) == 1
four (dups . fmap value -> (x : xs)) = (>= 2) $ length $ filter (== x) xs
four _ = False

sf :: HAND
sf h = straight h && flush h

hands :: [(String, Hand -> Bool)]
hands =
    [ ("straight flush   ", sf)
    , ("four of a kind   ", four)
    , ("full house       ", fullhouse)
    , ("flush            ", flush)
    , ("straight         ", straight)
    , ("three of a kind  ", three)
    , ("two pair         ", twopair)
    , ("pair             ", pair)
    , ("high card        ", const True)
    ]

runhands :: StateT [Hand] IO ()
runhands = sequence_ $ flip fmap hands $ \(n, p) -> do
    psb <- get
    put $ filter (not . p) psb
    let good = length $ filter p $ psb
    lift $ printf "%s %.06f%% (%i)\n" n (prob good) good

possible :: [Hand]
possible = nchands 5 deck

prob :: Int -> Double
prob n = 100 * fromInteger (toInteger n) / fromInteger (toInteger (length possible))

main :: IO ()
main = do
    evalStateT runhands possible
    return ()
