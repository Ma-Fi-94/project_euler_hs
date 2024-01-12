{-# LANGUAGE NumericUnderscores #-}

import Data.Fixed (mod')
import Data.Function (on)
import Data.List (maximumBy)
import Data.Ratio
import Utils (tok)


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
    input099 <- readFile "0099_base_exp.txt"
    --print $ "Problem 091: " ++ (show $ p091 50)

    --print $ "Problem 097: " ++ show p097
    --print $ "Problem 099: " ++ (show $ p099 input099)


    print $ "Problem 100: " ++ (show $ p100 1_000_000_000_001)

    print $ "---------- Done. ----------"