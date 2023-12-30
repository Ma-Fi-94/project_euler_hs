{-# LANGUAGE NumericUnderscores #-}

import Data.Char (digitToInt, isDigit)
import Data.List ((\\), intersect, maximumBy, nub, sort, tails)
import Data.Maybe
import Data.Ord (comparing)
import Data.Ratio
import Debug.Trace (trace)


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
-- Not elegant, but runs nearly instantly when compiled for speed.
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


-- Straightforward exhaustive search. Runs nearly instantly.
p036 n = sum . filter (pali . dec2bin) . filter (pali) $ [0..n-1]
  where
    pali x  = (show x) == (reverse (show x))
    dec2bin = reverse . go
      where
        go :: Int -> String
        go 0 = ""
        go i = show (i `rem` 2) ++ go (i `div` 2)


-- A clever way of constructing this list makes it run nearly instantly:
-- Because for every number d1.d2.d3..., all sublists d1, d1.d2, d1.d2.d3
-- must be prime, we start with the 4 single-digit prime numbers and add
-- digit after digit, retaining only prime numbers at every step. This
-- leads to a small set of right-truncatable primes, from which we then
-- only have to retain those primes that are also left-truncatable.
p037 = sum
     . filter isTruncL
     . filter (>9)
     $ go [2, 3, 5, 7]
  where
    go xs     = concat
              . takeWhile (not . null)
              $ iterate (concatMap step) xs
    step   x  = filter isPrime $ map ((+10*x)) [0..9]
    isTruncL  = all isPrime
              . map (read :: String -> Int)
              . tail . init . tails
              . (show :: Int -> String)
    isPrime i = (i > 1) && all ((>0) . (i `rem`)) [2..isqrt i]
    isqrt     = truncate . sqrt . fromIntegral


-- Optimised version of the previous naive approach. Reduced runtime
-- from 5.15s to 180-190ms.
-- The key insight here is that we have two constraints, namely that
-- p = a + b + c, and c^2 = a^2 + b^2. Thus, we only have one degree
-- of freedom: For any a we choose, b and c are exactly determined.
-- b = (p^2/2 - ap) / (p - a)
-- c = p - a - b.
-- If b and c happen to be integers, the solution is valid.
p039 = fst
      . maximumBy (comparing snd)
      . zip ([1..])
      . map (length . triangles)
      . enumFromTo 1 
  where
    triangles p = [(a, b, c) | a <- [1..(p - 2)],
                               let b = ((p^2 `div` 2) - a*p) `div` (p - a),
                               let c = p - a - b,
                               c^2 == a^2 + b^2]


-- Straightfoward, running instantly.
p040 = product . map (c!!) $ [0, 9, 99, 999, 9999, 99999, 999999]
  where
    c = map digitToInt . filter (isDigit) . show $ [1..]


main = do
    --print $ "Problem 031: " ++ show (p031 200)
    --print $ "Problem 032: " ++ show (p032)
    --print $ "Problem 033: " ++ show p033
    --print $ "Problem 034: " ++ show p034
    --print $ "Problem 035: " ++ show (p035 1_000_000)
    --print $ "Problem 036: " ++ show (p036 1_000_000)
    print $ "Problem 037: " ++ show p037
    --print $ "Problem 039: " ++ show (p039 1000)
    print $ "Problem 040: " ++ show p040
    
    print $ "---------- Done. ----------"