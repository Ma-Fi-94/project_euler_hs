{-# LANGUAGE NumericUnderscores #-}

import Control.Arrow (second)
import Data.Char (digitToInt)
import Data.Fixed (mod')
import Data.Function (on)
import Data.List (group, groupBy, maximumBy, sortBy)
import Data.Maybe (fromJust, isJust)
import Data.Ratio
import Utils (isPrime, primeFactors, tok)

import Data.Set (Set)
import qualified Data.Set as Set

import Debug.Trace (trace)


-- Slightly more optimised version of a previous, very naive approach.
-- In particular, element-wise tuple comparison frees us from a
-- costly 'nubBy'. Also dropped check for c^2 > 0, without speedup
-- though. Runs in less than half a second, compared to 2.5 seconds before.
p091 n = length $ [1 | x1 <- [0..n],
                       y1 <- [0..n],
                       x2 <- [0..n],
                       y2 <- [0..n],
                       (x1, y1) < (x2, y2),
                       let a2 = x1^2 + y1^2, a2 > 0,
                       let b2 = x2^2 + y2^2, b2 > 0,
                       let c2 = (y2 - y1) ^ 2 + (x2 - x1) ^ 2,
                       a2 + b2 == c2 || a2 + c2 == b2 || b2 + c2 == a2]


-- A simple trick makes this performant: First, calculate the squareSum.
-- This reduces the search space from 10M to 567. Then, we look up the
-- terminal number and memoise all intermediate result for speed.
p092 = length 
     . filter (==89) 
     . map (terminal . squareSum)
     $ [1..9_999_999]
  where
    terminal 1  = 1
    terminal 89 = 89
    terminal i  = memo !! (squareSum i)
    memo        = (-999) : map terminal [1..]
    squareSum   = sum . map (^2) . map digitToInt . show


-- The key for performance lies in an efficient calculation of the
-- sum of proper divisors, which is based on prime factorisations.
-- Compiled for speed, this runs in ~6sec on my machine.
p095 n = minimum
       . map fst
       . head
       . groupBy (\(_, a) (_, b) -> a == b)
       . reverse
       . sortBy (compare `on` snd)
       . map (second fromJust)
       . filter (isJust . snd)
       . zip [1..n] 
       $ map maybeCycleLen [1..n]
  where
    maybeCycleLen k = go (Set.empty) 0 k
      where
        go seen ctr cur
            | ctr > 0 && cur == k   = Just ctr
            | isPrime cur           = Nothing -- shaves of ~ 1 o.o.m.
            | cur > 1_000_000       = Nothing
            | cur `Set.member` seen = Nothing
            | otherwise             = go seen' (ctr + 1) cur'
              where
                seen'        = cur `Set.insert` seen
                cur'         = sumPropDiv cur
                sumPropDiv i = (+(-i))
                             . product
                             . map (\(p, e) -> sum [p ^ i | i <- [0..e]])
                             . map (\l -> (head l, length l))
                             . group
                             $ primeFactors i

-- It is, what it is.
p097 = (read :: String -> Int)
     . reverse . take 10 . reverse . show
     $ 28433 * 2^7830457 + 1


-- Direct calculation, using the fact that we can compare log-ed values.
-- This frees us from computing and comparing very long numbers.
p099 = fst
     . maximumBy (compare `on` snd)
     . zip [1..]
     . map (\l -> (fromIntegral (l!!1)) * log (fromIntegral (l!!0)))
     . map (map (read :: String -> Integer))
     . map (tok ",")
     . lines


-- I first tried all values of n, calculated the corresponding value of d,
-- and checked whether p would be exactly 0.5. This was correct, but extremely
-- inefficient and wouldn't find a solution within 15 minutes.
-- Let's look at this from another angle:
-- We want some (n,d) in N, s.t. b*(b-1) / (n*(n-1)) = 0.5.
-- This reduces to 2b^2 - n^2 - 2b + n = 0, a quadratic Diophantine equation.
-- From www.alpertron.com.ar/QUAD.HTM, we get that all solutions must satisfy
-- (b', n') = (3b + 2n - 2, 4b + 3n - 3), starting with (b=1, n=1).
-- Note that both b' and n' are increasing in both b and n. Also note
-- that for (1,1) we get new values (3,4). Thus, with every step, n will always
-- increase strictly. Thus, we may simply enumerate all solutions until we find
-- the first one where n >= nmin.
-- This runs in fractions of a second :).
p100 nmin = go 1 1
  where
     go b n
         |n >= nmin = b
         |otherwise = go b' n'
           where
               b' = 3 * b + 2 * n - 2
               n' = 4 * b + 3 * n - 3


main = do
    -- input099 <- readFile "0099_base_exp.txt"

    --print $ "Problem 091: " ++ (show $ p091 50)
    --print $ "Problem 092: " ++ (show p092)
    print $ "Problem 095: " ++ (show $ p095 1_000_000)
    --print $ sumPropDiv 96
    --print $ "Problem 097: " ++ show p097
    --print $ "Problem 099: " ++ (show $ p099 input099)


    -- print $ "Problem 100: " ++ (show $ p100 1_000_000_000_001)

    print $ "---------- Done. ----------"