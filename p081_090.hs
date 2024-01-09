{-# LANGUAGE NumericUnderscores #-}

import Data.List (nub)
import Data.Set (Set)
import qualified Data.Set as Set

-- Making use of the fact that 'primes' is ordered, as well as using 
-- a set for removing duplicate entries.
p087 n = Set.size $ Set.fromList [a + b + c | a <- takeWhile (<n) $ map (^2) ps,
                                              b <- takeWhile (<n) $ map (^3) ps,
                                              c <- takeWhile (<n) $ map (^4) ps,
                                              a + b + c < n]
  where
     ps        = filter isPrime [2..n]
     isqrt     = truncate . sqrt . fromIntegral
     isPrime n = all ((/=0) . (n  `rem`)) [2..isqrt n]

main = do
    print $ "Problem 087: " ++ show (p087 50_000_000)

    print $ "---------- Done. ----------"
