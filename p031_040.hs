{-# LANGUAGE NumericUnderscores #-}

import Data.Char (digitToInt)


-- Just counting the number of possibilities, and looking at
-- every coin only once (in descending order) makes this run
-- nearly instantly. (I first tried a naive brute-force /
-- enumerative approach without any success.)
p031 = go [200, 100, 50, 20, 10, 5, 2, 1]
  where
    go _ 0  = 1
    go [] _ = 0
    go (c:cs) rest = sum $ map (\i -> go cs (rest - i * c)) choices
      where
        choices = [0..(rest `div` c)]


-- Note that 9! = 362880, so a number with d digits can map to
-- at most d*362880. Hence, a number with 8 digits can map to
-- at most 2903040, which has only seven digits. Thus, we need
-- to only explore numbers up to 10_000_000.
-- We start our search at 10 to exclude trivial cases.
-- Runs in less than two seconds (when compiled for speed),
-- which is acceptable.
p034 = sum . filter check $ [10..10_000_000]
  where
    check i = (==i) . sum . map (fac . digitToInt) . show $ i
    fac 0   = 1
    fac i   = foldl1 (*) [1..i]


-- We exclude the cases below 100 (= 13 primes), so we only check
-- 100 to 999_999. Thus, we can a priori exclude all numbers
-- that contain a 2,4,6,8,0 ("invalid numbers").
p035 i
  |i < 10    = error "Not implemented."
  |otherwise = (+13)
             . length
             . filter (all isPrime . rots)
             . filter valid
             $ enumFromTo 100 (i - 1)
  where
    valid = all odd . map digitToInt . show

    isPrime i = all ((>0) . (i `rem`)) [2..isqrt i]
      where
        isqrt = truncate . sqrt . fromIntegral

    rots :: Int -> [Int]
    rots n = go len dblstr
      where
        len      = length . show $ n 
        dblstr   = concat . replicate 2 . show $ n
        go 0 str = []
        go c str = (read (take len str)) : go (c - 1) (tail str)


main = do
    -- print $ "Problem 031: " ++ show (p031 200)

    -- print $ "Problem 034: " ++ show p034
    print $ "Problem 035: " ++ show (p035 1_000_000)
    
    print $ "---------- Done. ----------"