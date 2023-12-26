-- The fastest native Fibonacci memoisation I know.
fib = 1 : 2 : zipWith (+) fib (tail fib)


-- A simple prime sieve. Rather inefficient.
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs,
                                  x `rem` p > 0]


-- Prime factorisation via trial divisions.
-- If we found a factor, we save it, and then we
-- divide it out *completely* before checking the
-- next candidate. In this way, we'll end up only
-- with prime factors.
primeFactors = go (2:[3,5..])
  where
    go (p:ps) n
        |p^2 > n        = [n]
        |n `mod` p == 0 = p : go (p:ps) (n `div` p)
        |otherwise      = go ps n


-- Primality check via naive trial division. This
-- has a worse asymptotic complexity than the sieves,
-- however its prefactor seems to be more benign.
-- This might be due to the lower memory requirements?
isPrime n = try [2..isqrt n]
  where
    isqrt      = truncate . sqrt . fromIntegral
    try []     = True
    try (t:ts) = if   n `mod` t == 0
                 then False
                 else try ts

