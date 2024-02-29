{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Control.Spoon (spoon)
import Data.Bifunctor (first)
import Data.Function (on)
import Data.Ratio (denominator, numerator, (%))
import qualified Data.Set as Set
import Debug.Trace (trace)
import System.IO (hFlush, stdout)
import Text.Printf (printf)

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

t :: (Show a) => String -> a -> a
t f x = if dbg then trace (printf "%s %s" f (show x)) x else x

wr :: (Show a) => String -> a -> Bool
wr s x = not dbg || trace (printf "%s %s" s (show x)) True

-- Quad a True b c = (a-√b)/c
-- Quad a False b c = (a+√b)/c
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
    show (Rational (x : xs)) = "[" ++ show x ++ "; " ++ drop 1 (show xs)
    show (Rational []) = "[]"
    show (Irrational (p : ps) r) = "[" ++ show p ++ "; " ++ init (drop 1 (show ps)) ++ "; " ++ drop 1 (show r)
    show (Irrational [] r) = "[;;" ++ drop 1 (show r)

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
rcp q@(Quad n True r d) | wr "rcp" q = normalize <$> if r /= n * n then return (Quad (d * n) False (d * d * r) (n * n - r)) else Nothing

-- d / n-√r * n+√r / n+√r = (dn+d√r) / (n²-r)

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

poly :: Quad -> String
poly (Quad a n b c) =
    let (a', b') = (a % c, b % (c ^ 2))
     in let p = 2 * a'
         in let q = b' - a' * a'
             in let d = (lcm `on` denominator) p q
             in let p' = -numerator (p * (d % 1))
             in let q' = -numerator (q * (d % 1))
                 in printf
                        ( "Monic in ℚ[x]: x² + (%s)x + (%s)"
                        ++ "\nNon-monic in ℤ[x]: %ix² %+ix %+i"
                            ++ "\nL.R. of the generalized fibonacci where p = %s, & q = %s"
                        )
                        (show' $ -p)
                        (show' $ -q)
                        d
                        p'
                        q'
                        (show' p)
                        (show' q)
  where
    show' :: Rational -> String
    show' x = if abs (denominator x) == 1 then show (denominator x * numerator x) else show x

main :: IO ()
main = do
    putStrLn "This calculator assumes a number in the form (a+√b)/c"
    a <- getInteger "What is a? "
    b <- getInteger "What is b? "
    c <- getInteger "What is c? "
    putStrLn ""
    printf "your surd is approximately %f" ((fromInteger a + (signum (fromInteger b) * sqrt (abs $ fromInteger b))) / fromInteger c :: Double)
    putStrLn ""
    let q = normalize $ Quad a (b < 0) (abs b) c
    let f = cf q
    print f
    putStrLn "\npolynomials:"
    putStrLn $ poly q
