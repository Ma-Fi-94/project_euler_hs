{-# LANGUAGE NumericUnderscores #-}

import Data.Function (on)
import Data.List (group, maximumBy)
import Data.Ratio
import Utils (primeFactors)


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


main = do
    print $ "Problem 069: " ++ show (p069 1_000_000)


    print $ "---------- Done. ----------"