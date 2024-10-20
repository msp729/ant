module Main (main) where

import Control.Monad
import Data.Ratio
import Text.Printf (printf)

main :: IO ()
main = do
    print $ toDouble $ est $ 10_000_000
    print $ toDouble $ quad 10 (Interval 0 1) (Interval 0 1)

bin_under :: (Integral a) => (a -> a) -> a -> a -> a -> a
bin_under f target = helper
  where
    helper lo hi
        | hi - lo == 1 = lo
        | otherwise =
            let mid = lo + div (hi - lo) 2
             in if f mid <= target
                    then helper mid hi
                    else helper lo mid

isqrt :: (Integral a) => a -> a
isqrt x = bin_under (join (*)) x 0 x

toDouble :: Rational -> Double
toDouble q = fromInteger (numerator q) / fromInteger (denominator q)

est :: Integer -> Rational
est = (4 *) . qpi

qpi :: Integer -> Rational
qpi n = let (m, i) = integrate n in (2 * i - m * (m + 1)) % (n * n)

integrate :: Integer -> (Integer, Integer)
integrate n = let (m, i) = helper 1 n 0 in (m, div (n + m) 2 + i)
  where
    helper :: Integer -> Integer -> Integer -> (Integer, Integer)
    helper x y s
        | x > y = (x, s)
        | x ^ 2 + y ^ 2 > n ^ 2 + n = helper x (y - 1) s
        | otherwise = helper (x + 1) y (y + s)

data Interval = Interval {lo, hi :: Rational}

instance Num Interval where
    a + b = Interval (lo a + lo b) (hi a + hi b)
    a * b
        | lo a >= 0 && lo b >= 0 = Interval (lo a * lo b) (hi a * hi b)
        | lo a >= 0 && hi b <= 0 = Interval (hi a * lo b) (lo a * hi b)
        | lo a >= 0 = Interval (hi a * lo b) (hi a * hi b)
        | hi a <= 0 && hi b <= 0 = Interval (lo a * lo b) (hi a * hi b)
        | hi a <= 0 && lo b >= 0 = Interval (hi b * lo a) (lo b * hi a)
        | hi a <= 0 = Interval (hi b * lo a) (lo b * lo a)
        | lo b >= 0 = Interval (hi b * lo a) (hi b * hi a)
        | hi b <= 0 = Interval (lo b * hi a) (lo b * lo a)
        | otherwise = Interval (min (lo a * hi b) (hi a * lo b)) (max (lo a * lo b) (hi a * hi b))
    abs a
        | lo a >= 0 = a
        | hi a <= 0 = Interval (abs (hi a)) (abs (lo a))
        | otherwise = Interval 0 (max (abs (hi a)) (abs (lo a)))
    fromInteger n = Interval (fromInteger n) (fromInteger n)
    negate x = Interval (negate (hi x)) (negate (lo x))
    signum x = Interval (signum (lo x)) (signum (hi x))

split :: Interval -> (Interval, Interval)
split x = let m = (lo x + hi x) / 2 in (Interval (lo x) m, Interval m (hi x))

quad :: Integer -> Interval -> Interval -> Rational
quad 0 x y
    | hi (x ^ 2 + y ^ 2) <= 1 = 1
    | lo (x ^ 2 + y ^ 2) >= 1 = 1
    | otherwise = 1 / 2
quad n x y
    | hi (x ^ 2 + y ^ 2) <= 1 = 1
    | lo (x ^ 2 + y ^ 2) >= 1 = 1
    | otherwise =
        let
            (x1, x2) = split x
            (y1, y2) = split y
         in
            (quad (n - 1) x1 y1 + quad (n - 1) x1 y2 + quad (n - 1) x2 y1 + quad (n - 1) x2 y2) / 4
