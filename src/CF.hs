{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Control.Spoon (spoon, teaspoon)
import Data.Bifunctor (first)
import Data.Function (on)
import Data.Ratio ((%))
import qualified Data.Set as Set
import Debug.Trace (trace)
import System.IO (hFlush, stdout)
import Text.Printf

dup :: (Ord a) => [a] -> Maybe a
dup xs = dup' xs Set.empty
  where
    dup' [] _ = Nothing
    dup' (x : xs) s =
        if Set.member x s
            then Just x
            else dup' xs (Set.insert x s)

dbg :: Bool
dbg = False

t :: Show a => String -> a -> a
t f x = if dbg then trace (printf "%s %s" f (show x)) x else x

wr :: Show a => String -> a -> Bool
wr s x = not dbg || trace (printf "%s %s" s (show x)) True

data Quad = Quad
    { num :: Integer
    -- ^ numerator
    , neg :: Bool
    , disc :: Integer
    -- ^ discriminant
    , den :: Integer
    -- ^ denominator
    }
    deriving (Ord, Eq)

data CF = Rational [Integer] | Irrational {prefix :: [Integer], rep :: [Integer]} deriving (Ord, Eq)

instance Show CF where
    show (Rational (x : xs)) = "[" ++ show x ++ "; " ++ tail (show xs)
    show (Rational []) = "[]"
    show (Irrational (p : ps) r) = "[" ++ show p ++ "; " ++ init (tail (show ps)) ++ "; " ++ tail (show r)
    show (Irrational [] r) = "[;;" ++ tail (show r)

instance Show Quad where
    show (Quad n b r d) = printf "(%i%s√%i)/%i" n (if b then "-" else "+") r d

normalize, normalize1, normalize2 :: Quad -> Quad
normalize = normalize1 . normalize2 . t "norm"
normalize1 (Quad n b r d) =
    let c = gcd n d
     in case divMod r (c ^ 2) of
            (r', 0) -> Quad (div n c) b r' (div d c)
            _ -> Quad n b r d
normalize2 q = if den q < 0 then q{neg = not $ neg q, num = negate $ num q, den = negate $ den q} else q

cfstep :: Quad -> (Integer, Maybe Quad)
cfstep (Quad n b r d) | wr "cfstep" (Quad n b r d) = let ip = floor ((fromInteger n + (if b then -1 else 1) * sqrt (fromInteger r)) / fromInteger d) in (ip, rcp (Quad (n - d * ip) b r d))

rcp :: Quad -> Maybe Quad
rcp q@(Quad n False r d) | wr "rcp" q = normalize <$> if r /= n * n then return (Quad (-d * n) False (d * d * r) (r - n * n)) else Nothing
rcp q@(Quad n True r d) | wr "rcp" q = normalize <$> if r /= n * n then return (Quad (d*n) False (d*d*r) (n*n-r)) else Nothing
-- d / n-√r * n+√r / n+√r

-- | Left means fraction, Right means irrational
cf :: Quad -> CF
cf x =
    let entries = lst x
     in let boundary = dup $ fst <$> entries
         in case boundary of
                Just rep -> uncurry Irrational $ sides rep entries
                Nothing -> Rational $ snd <$> entries
  where
    lst :: Quad -> [(Quad, Integer)]
    -- List of quads and their integer parts s.t. the next is the reciprocal of the fractional part
    -- e.g. [(φ,1), (φ,1), ...] (because 1/(φ-1) = φ)
    -- or [((1+√0)/1, 1)] (because it has no fractional part)
    -- or [((3+√0)/2, 1), ((2+√0)/1, 2)]
    lst x@(cfstep -> (n, mq)) = (x, n) : maybe [] lst mq

    sides :: Quad -> [(Quad, Integer)] -> ([Integer], [Integer])
    sides q [] = error "this should be impossible"
    sides q ((x, n) : lst)
        | q == x = ([], (n :) $ snd <$> takeWhile ((/= q) . fst) lst)
        | otherwise = first (n :) $ sides q lst

getInteger :: String -> IO Integer
getInteger prompt = do
    putStr prompt
    hFlush stdout
    (spoon . read -> m) <- getLine
    case m of
        Just n -> return n
        Nothing -> getInteger prompt -- keep asking if we don't get anything

main :: IO ()
main = do
    putStrLn "This calculator assumes a number in the form (a+√b)/c"
    a <- getInteger "What is a? "
    b <- getInteger "What is b? "
    c <- getInteger "What is c? "
    let q = normalize $ Quad a (b < 0) (abs b) c
    let f = cf q
    print f
