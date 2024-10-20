module Main (main) where

force :: a -> a
force x = seq x x
force' :: a -> ()
force' x = x `seq` ()

main :: IO ()
main = mapM_ (return . force' . clever) [2_000_000..2_000_100]

type Fib = forall a. (Integral a) => a -> a
type Fib' = forall a. (Integral a) => Int -> a

{-
 - for input n and output m, I believe they perform (for single calls, assuming arithmetic is O(1)) as:
 - evil: O(m)
 - cute: O(n²)
 - clever, pretty: O(n)
 - genius: O(log(n))
 - note that evil is unsuited to any calculations greater than 30.
 - also note that for multiple calls, cute (?) and pretty will benefit from their storing of previous (and intermediate) results.
 - clever takes up less memory, though.
 - genius is just so much faster than the others that it kinda doesn't matter
 -}

-- evil (2 ^ 5 + 3) took about 200 ms
evil :: Fib
evil 0 = 0
evil 1 = 1
evil n
    | n > 0 = evil (n - 1) + evil (n - 2)
    | n < 0 = evil (n + 2) - evil (n + 1)

-- cute (2 ^ 13) took about 200 ms
cute :: Fib'
cute = (map helper [0 ..] !!)
  where
    helper 0 = 0
    helper 1 = 1
    helper n = cute (n - 1) + cute (n - 2)

-- pretty (2 ^ 18) took about 400 ms
pretty :: Fib'
pretty = (fibs !!)
  where
    fibs = 0 : fibs'
    fibs' = 1 : zipWith (+) fibs fibs'

-- clever (2 ^ 18) took about 400 ms
clever :: Fib
clever a = helper 0 1 a
  where
    helper a b 1 = b
    helper a b n
        | n > 0 = helper b (a + b) (n - 1)
        | otherwise = helper (b - a) a (n + 1)

-- genius (2 ^ 23) took about 250 ms
genius :: Fib
genius n = x ((fmat ^ n) @*^ f01)

data V2 a = V2 {x :: a, y :: a}
data M2 a = M2 {i :: V2 a, j :: V2 a}

f01 :: (Num a) => V2 a
f01 = V2 0 1

fmat :: (Num a) => M2 a
fmat = M2 (V2 0 1) (V2 1 1)

(+^) :: (Num a) => V2 a -> V2 a -> V2 a
V2 a b +^ V2 a' b' = V2 (a + a') (b + b')

(*^) :: (Num a) => a -> V2 a -> V2 a
s *^ v = V2 (s * x v) (s * y v)

(@*^) :: (Num a) => M2 a -> V2 a -> V2 a
M2 i j @*^ V2 x y = (x *^ i) +^ (y *^ j)

instance (Num a) => Num (M2 a) where
    M2 i j + M2 i' j' = M2 (i +^ i') (j +^ j')
    m * M2 i j = M2 (m @*^ i) (m @*^ j)
    fromInteger n = M2 (V2 (fromInteger n) 0) (V2 0 (fromInteger n))
    abs = id
    signum = const 1
    negate (M2 (V2 a c) (V2 b d)) = M2 (V2 (-a) (-c)) (V2 (-b) (-d))
