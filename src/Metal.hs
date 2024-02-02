{-# LANGUAGE ViewPatterns #-}
module Main (main) where

import Text.Printf
import System.IO
import Data.Ratio

mseq :: Integer -> Integer -> [Integer]
mseq p q = 1:1:zipWith (\a b -> p * b + q * a) (mseq p q) (tail $ mseq p q)

nseq :: Double -> Double -> [Double]
nseq p q = 1:1:zipWith (\a b -> (a-p*b)/q) (nseq p q) (tail $ nseq p q)

unplus :: Char -> Char
unplus '+' = ' '
unplus x = x

main :: IO ()
main = do
    putStr "What is p? "
    hFlush stdout
    (read -> p :: Integer) <- getLine
    putStr "What is q? "
    hFlush stdout
    (read -> q :: Integer) <- getLine

    let ms = mseq p q
    let pc = zip [1..] $ zip ms $ tail ms
    let p' = fromInteger p
    let q' = fromInteger q
    let r :: Double = (p' + sqrt (p' * p' + 4 * q')) / 2

    printf "r = (%i+√%i)/2 = %.8f\n" p (p*p + 4*q) r
    putStrLn "  n  |          xₙ         |     rₙ     | relative error (logarithmic)"
    sequence_ $ do
        (i :: Int, (p, c)) <- take 30 pc
        let r' = (fromInteger c / fromInteger p :: Double)
        return $ printf " % 3i | % 19i | %.8f | %.6f\n" i p r' $ log $ abs $ (r' - r) / r

    putStrLn ""
    putStrLn ""
    putStrLn $ replicate 50 '#'
    putStrLn ""
    putStrLn ""

    let ms = nseq p' q'
    let pc = zip [2,1..] $ zip ms $ tail ms
    let r :: Double = (p' - sqrt (p' * p' + 4 * q')) / 2

    printf "conjugate r = %.8f\n" r
    putStrLn "  n  |      xₙ     |     rₙ     | relative error (logarithmic)"
    sequence_ $ do
        (i :: Int, (p, c)) <- take 33 pc
        let r' = p / c
        return $ putStrLn $ map unplus $ printf " % 3i | %+.11f | %.8f | %.6f" i p r' $ log $ abs $ (r' - r) / r
