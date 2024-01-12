{-# LANGUAGE NumericUnderscores #-}

import Data.Ratio

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

main = do
    print $ "Problem 071: " ++ show (p071 (3 % 7) 1_000_000)

    print $ "---------- Done. ----------"