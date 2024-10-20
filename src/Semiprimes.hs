module Main (main) where

import Control.Monad (join)

main :: IO ()
main = do
    print $ length $ takeWhile (<= cap) alt_ssqs
    print $ sum $ map (length . takeWhile (<= cap) . semisquares) $ takeWhile (<= cap) primes
    print $ conjecture cap

{-
    print $
        sum $
            map length $
                takeWhile ((> 0) . length) $
                    map (takeWhile (< cap) . semisquares) primes
-}

semisquares :: (Integral a) => a -> [a]
semisquares p = (\n -> p * n * n) <$> [1 ..]

fiti :: (Integral a, Num b) => a -> b
fiti = fromInteger . toInteger

cap :: Int
cap = 1_000

divisible :: (Integral a) => a -> a -> Bool
divisible a b = a `mod` b == 0

isPrime :: (Integral a) => a -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime 3 = True
isPrime n = not $ any (\p -> n `divisible` p) $ takeWhile (\p -> p * p <= n) primes

primes :: (Integral a) => [a]
primes = 2 : 3 : filter isPrime ([1 ..] >>= (\n -> [6 * n - 1, 6 * n + 1]))

alternate :: (Integral a) => a -> Bool
alternate n = flip any (takeWhile (<= n) primes) $ \p -> divisible n p && isSquare (div n p)

isSquare :: (Integral a) => a -> Bool
isSquare n = any ((== n) . join (*)) $ takeWhile ((<= n) . join (*)) [1 ..]

alt_ssqs :: (Integral a) => [a]
alt_ssqs = filter alternate [1 ..]

isqrt :: (Integral a) => a -> a
isqrt t = helper 0 t
  where
    helper lo hi
        | lo * lo == t = lo
        | hi * hi == t = hi
        | lo + 1 == hi = lo
        | otherwise =
            let mid = lo + div (hi - lo) 2
             in if mid * mid > t then helper lo mid else helper mid hi

conjecture :: (Integral a) => a -> a
conjecture n = sum $ map (\p -> isqrt (n `div` p)) $ takeWhile (<= n) primes
