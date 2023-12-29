{-# LANGUAGE NumericUnderscores #-}

import Data.Char (digitToInt, isDigit)
import Data.List ((\\), intersect, maximumBy, nub, sort)
import Data.Maybe
import Data.Ord (comparing)
import Data.Ratio

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


-- Bruteforce with some constraints to limit the search space.
p032 = sum . nub $ [c | a <- [1..2_000],
                        b <- [1..2_000],
                        a < b,
                        let c = a * b,
                        null $ (show a) `intersect` (show b),
                        null $ (show a) `intersect` (show c),
                        null $ (show b) `intersect` (show c),                        
                        pan9 (show a ++ show b ++ show c)]
  where
    pan9 :: String -> Bool
    pan9 = (=="123456789") . sort


-- Exhaustive search with some constraints, runs instantly.
p033 = denominator
     . product
     . map (\(n, d) -> n % d) 
     . filter test
     $ candidates
  where
    candidates      = [(n, d) | n <- [10..99],
                                d <- [10..99],
                                n < d,
                                isJust (sharedDigit n d)]
    sharedDigit i j = if   length ((show i) `intersect` (show j)) == 1
                      then Just . head $ (show i) `intersect` (show j)
                      else Nothing
    test (i, j)     = (j' /= 0) && (i % j == i' % j') && (not trivial)
      where
        i'      = read . (\\[fromJust (sharedDigit i j)]) $ show i
        j'      = read . (\\[fromJust (sharedDigit i j)]) $ show j
        trivial = i `rem` 10 == 0 && j `rem` 10 == 0 
    

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

    rots n = go len dblstr
      where
        len      = length . show $ n 
        dblstr   = concat . replicate 2 . show $ n
        go 0 str = []
        go c str = (read (take len str)) : go (c - 1) (tail str)


-- Bruteforce with constraints. Takes a bit of time,
-- so there's some more room for optimisation, I think.
p039 = fst
     . maximumBy (comparing snd)
     . zip ([1..])
     . map (length . triangles)
     . enumFromTo 1 
  where
    triangles p = [(a, b, c) | a <- [1..(p - 2)],
                               b <- [1..(p - a - 1)],
                               let c = p - a - b,
                               c^2 == a^2 + b^2]


-- Straightfoward
p040 = (c!!0) * (c!!9) * (c!!99) * (c!!999) * (c!!9999) * (c!!99999) * (c!!999999)
  where
    c = map digitToInt . filter (isDigit) . show $ [1..]


main = do
    -- print $ "Problem 031: " ++ show (p031 200)
    print $ "Problem 032: " ++ show (p032)
    -- print $ "Problem 033: " ++ show p033
    -- print $ "Problem 034: " ++ show p034
    -- print $ "Problem 035: " ++ show (p035 1_000_000)

    -- print $ "Problem 039: " ++ show (p039 1000)

    -- print $ "Problem 040: " ++ show p040
    
    print $ "---------- Done. ----------"