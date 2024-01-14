{-# LANGUAGE NumericUnderscores #-}

import Data.Char (digitToInt)
import Data.List (nub)
import Data.Maybe (fromJust)
import Data.Ratio
import Data.Set (Set)
import qualified Data.Set as Set
import Utils (primeFactors)

-- Binary search on the Stern-Brocot tree. Runs virtually instantly.
p071 x dmax = go (0 % 1) (1 % 1)
  where
    go l r
        | denominator m >= dmax = numerator l
        | m >= x                = go l m
        | m <  x                = go m r
          where
            m  = (n' % d')
            n' = (numerator l) + (numerator r)
            d' = (denominator l) + (denominator r)


-- We only need to sum over the Euler phi function phi(d) for d = 2...dmax.
-- Due to Möbius, we have an efficient formula for this, which we will use.
-- Because this formula includes the case of k=1, we need to subtract one
-- from its result.
p072 dmax = totientSum dmax - 1
  where
    totientSum n = (sum (map f [1..n])) `div` 2
    f k          = (mu k) * (dmax `div` k) * (1 + (dmax `div` k))
    mu k
        | k == 1                             = 1
        | primefactors /= (nub primefactors) = 0
        | even (length primefactors)         = 1
        | odd (length primefactors)          = (-1)
          where
            primefactors = primeFactors k


-- We know that the maximum chain is of length 60, so we take the first 60
-- iterations and check if all elements are unique.
-- This runs in 22s when compiled for speed, which is acceptable.
p074 = length . filter allUniq . map (take 60 . iterate sumFac) $ [1..999_999]
  where
    allUniq (x:[]) = True
    allUniq (x:xs) = if   x `elem` xs
                     then False
                     else allUnique xs
    sumFac         = sum . map (fac . digitToInt) . show
    fac 0          = 1
    fac n          = foldl1 (*) $ enumFromTo 1 n


main = do
    -- print $ "Problem 071: " ++ show (p071 (3 % 7) 1_000_000)
    -- print $ "Problem 072: " ++ show (p072 1_000_000)
    print $ "Problem 074: " ++ show p074


    print $ "---------- Done. ----------"