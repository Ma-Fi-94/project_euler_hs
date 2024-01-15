{-# LANGUAGE NumericUnderscores #-}

import Data.Function (on)
import Data.List (group, maximumBy, minimumBy, sort)
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
    --print $ "Problem 069: " ++ show (p069 1_000_000)
    print $ "Problem 070: " ++ show (p070 10_000_000)


    print $ "---------- Done. ----------"