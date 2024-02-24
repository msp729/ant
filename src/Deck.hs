module Main (main) where

import Text.Printf

data Value
    = Ace
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
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
nchands 1 d = (:[]) <$> d
nchands n (x:xs) = ((x:) <$> nchands (n-1) xs) ++ nchands n xs
nchands n [] = []

heartcond :: Hand -> Bool
heartcond = (== 2) . length . filter isheart

facecond :: Hand -> Bool
facecond = (== 2) . length . filter isface

main :: IO ()
main = do
    let hands = nchands 5 deck
    let either = filter (liftA2 (||) heartcond facecond) hands
    let hearts = filter heartcond either
    let faces = filter facecond either
    let both = filter heartcond faces
    printf "total hands: %i\n" (length hands)
    printf "either: %i\n" (length either)
    printf "hearts: %i\n" (length hearts)
    printf "faces: %i\n" (length faces)
    printf "both: %i\n" (length both)
