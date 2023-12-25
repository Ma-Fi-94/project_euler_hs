{-# LANGUAGE NumericUnderscores #-}

import qualified Data.Set as Set
import Data.Char (digitToInt)


-- Avoiding costly divisibility checks by
-- generating multiples and removing
-- duplicates via a set.
p001 n = sum
       . Set.fromList
       $ map (*3) [1..(n - 1) `div` 3]
      ++ map (*5) [1..(n - 1) `div` 5]


-- The fastest native Fibonacci memoisation I know.
p002 n = sum
     . filter even 
     . takeWhile (<=n)
     $ fib
  where
    fib = 1 : 2 : zipWith (+) fib (tail fib)


-- We only need to factor the number into primes,
-- instead of generating primes (which is costly).
-- If we found a factor, we save it, and then we
-- divide it out *completely* before cchecking the next candidate.
-- In this way, we'll end up only with prime factors.
p003 = last . primeFactors
  where
    primeFactors = go (2:[3,5..])
      where
        go (p:ps) n
            |p^2 > n        = [n]
            |n `mod` p == 0 = p : go (p:ps) (n `div` p)
            |otherwise      = go ps n


-- Using symmetry of multiplication.
p004 m n = maximum
       . filter isPali 
       $ [n1 * n2 | n1 <- [m..n],
                    n2 <- [m..n],
                    n1 > n2]
  where
    isPali x = (show x) == reverse (show x)



-- This feels a bit like cheating. The "manual" approach
-- would be to decompose all factors into primes factors.
-- Then, for every prime number pick the highest exponent,
-- and multiply them together.
-- For the example, this would yield
-- 2^4 * 3^2 * 5 * 7 * 11 * 13 * 17 * 19.
p005 = foldl1 lcm . enumFromTo 1


-- Using well-known closed-form solutions,
-- which are trivial to prove by induction.
p006 n = (n * (n + 1) `div` 2) ^ 2
       - n * (n + 1) * (2 * n + 1) `div` 6


-- Primality check via naive trial division. This
-- has a worse asymptotic complexity than the sieves,
-- however its prefactor seems to be more benign.
-- This might be due to the lower memory requirements?
p007 n = last . take n . filter isPrime $ [2..]
  where
    isPrime n = try [2..isqrt n]
      where
        isqrt      = truncate . sqrt . fromIntegral
        try []     = True
        try (t:ts) = if   n `mod` t == 0
                     then False
                     else try ts


-- Straighforward recursion over a string.
p008 = maximum . go . show
  where
    go xs
        |length xs <= 13 = [product (map digitToInt xs)]
        |otherwise       = product (take 13 (map digitToInt xs))
                         : go (tail xs)


-- Enough constraints make this fast.
p009 n = head [a * b * c | a <- [1..n],
                           b <- [1..n],
                           let c = n - a - b,
                           a < b,
                           b < c,
                           a ^ 2 + b ^ 2 == c ^ 2]


-- Primality check via naive trial division, see comment
-- to p007.
p010 = sum . filter isPrime . enumFromTo 2
  where
    isPrime n = try [2..isqrt n]
      where
        isqrt      = truncate . sqrt . fromIntegral
        try []     = True
        try (t:ts) = if   n `mod` t == 0
                     then False
                     else try ts


main = do
    print $ "Problem 001: " ++ show (p001 1000)
    print $ "Problem 002: " ++ show (p002 4_000_000)
    print $ "Problem 003: " ++ show (p003 600_851_475_143)
    print $ "Problem 004: " ++ show (p004 100 999)
    print $ "Problem 005: " ++ show (p005 20)
    print $ "Problem 006: " ++ show (p006 100)
    print $ "Problem 007: " ++ show (p007 10001)
    print $ "Problem 008: " ++ show (p008 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450)
    print $ "Problem 009: " ++ show (p009 1000)
    print $ "Problem 010: " ++ show (p010 2_000_000)


