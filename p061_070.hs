{-# LANGUAGE NumericUnderscores #-}

import Data.Char (digitToInt)
import Data.Function (on)
import Data.List ((\\), elemIndex, group, maximumBy, minimumBy, sort)
import Data.Maybe (fromJust)
import Data.Ratio
import Utils (primeFactors)


-- We first enumerate all cubes n^3 for n <= 10_000. To check for
-- permutations, we then sort the digits of all cubes, yielding
-- cubes'. We sort this, group, and get the first element that's
-- present exactly 5 times. Then we just have to look up the
-- index of this element and raise it to the third power.
-- Runs virtually instantly, since the search space is small.
p062 = idx ^ 3
  where
    idx    = fromJust $ elemIndex first cubes'
    first  = head . head . filter ((==5) . length) . group . sort $ cubes'
    cubes' = map (sort . show) cubes 
    cubes  = map (^3) [0..10_000]


-- We want all n-digit numbers x, which are the n-th power of some
-- positive number b. If there are no constraints on b, there exists no
-- upper limit on n above which no solutions exist. Hence, I assumed
-- the problem failed to mention that b must be a single-digit number,
-- i.e. 1 <= b <= 9.
-- First, we show there exists an N, s.t. for all n>N all n-th powers we
-- can maximally reach (when b=9) have less than n digits. We have:
-- 9^N < 10^(N-1), which can be directly solved to N ~ 21.854.
-- Thus, we only need to search for n in 1..22. The rest is a simple
-- enumeration of all n-th powers and filtering those with n digits.
p063 = length [1 | b <- [1..9],
                   n <- [1..22],
                   b ^ n >= 10 ^ (n - 1),
                   b ^ n <= 10 ^ n - 1]


-- Straightforward direct calculation thanks to the Ratio type.
p065 = sum . map digitToInt . show . numerator . build $ e'
  where
    build (x:[]) = (x % 1)
    build (x:xs) = (x % 1) + (1 / (build xs))
    e'           = reverse . take 100 $ e
    e            = 2 : concat [[1, 2 * k, 1] | k <- [1..]]


-- Using prime factorisation (by repeated trial division) for
-- efficient calulcation of phi. Compiled for speed, this
-- runs in ~1s, which I find impressive.
p069 = fst
     . maximumBy (compare `on` snd)
     . map (\n -> (n, n % phi n))
     . enumFromTo 2
  where
    phi = foldl1 (*)
        . map (\ps -> (head ps) ^ ((length ps) - 1) * ((head ps) - 1))
        . group 
        . primeFactors


-- Like before, but we are now looking for the minimum ratio!
-- We also add the constraint of phi (n) and n being permutations.
-- We check this efficiently by sorting their digits and testing
-- for equality. Runs in 20sec, which is okayish.
p070 = fst
     . minimumBy (compare `on` snd)
     . map (\(n, p) -> (n, n % p))
     . filter isPerm
     . map (\n -> (n, phi n))
     . enumFromTo 2
  where
    isPerm (n, p) = sort (show n) == sort (show p)
    phi           = foldl1 (*)
                  . map (\ps -> (head ps) ^ ((length ps) - 1) * ((head ps) - 1))
                  . group 
                  . primeFactors

main = do
    --print $ "Problem 062: " ++ show (p062)
    print $ "Problem 063: " ++ show (p063)
    -- print $ "Problem 065: " ++ show (p065)
    --print $ "Problem 069: " ++ show (p069 1_000_000)
    --print $ "Problem 070: " ++ show (p070 10_000_000)


    print $ "---------- Done. ----------"