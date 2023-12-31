{-# LANGUAGE NumericUnderscores #-}

import Data.Char (ord)
import Data.Fixed (mod')
import Data.Ord (comparing)
import Data.List (group, inits, intersect, maximumBy, nub, permutations, sort, tails)
import Utils (tok, isPrime, primeFactors, readInt)

-- Exhaustive search, based on first generating all n-digit
-- pandigital numbers for n = 1..9. Runs nearly instantly.
p041 = head . reverse . sort . filter isPrime
     . map readInt . foldl1 (++) . map permutations . tail . inits
     $ "123456789"


-- The maximum word score is 192, so we just generate the first 200
-- triangle numbers and check how many words have a score that's
-- element of that list. An obious improvement would be to use
-- the quadratic formula to check whether a score is a triangle number.
-- However, given the tiny search space this is not required at all.
p042 = length . filter (`elem` triangles) . map wordScore . tok ['"', ',']
  where
    wordScore = sum . map ((subtract 64) . ord)
    triangles = map (\n -> n * (n + 1) `div` 2) [1..200]


-- Straightfoward enumeration and check. Runtime of about 6s, which
-- is not perfect, but still acceptable in my opinion.
p043 = sum $ filter test candidates
  where
    test x     = and
               . map (\(i, d) -> (subNb i (i+2) x) `rem` d == 0)
               $ zip [1..7] [2, 3, 5, 7, 11, 13, 17]
    subNb a b  = readInt . take (b - a + 1) . drop a . show
    candidates = filter (>=10^9)
               . map readInt
               $ permutations "0123456789"

-- W.l.o.g. we assume j > k; pentagonality check as described below.
-- This makes it run within a few seconds.
p044 = head $ [(penta j - penta k)| j <- [2..10000],
                                    k <- [j-1, j-2..1],
                                    isPenta (penta j + penta k),
                                    isPenta (penta j - penta k)]
  where
    penta n   = n * (3 * n - 1) `div` 2 
    isPenta x = (1 + sqrt (24 * (fromIntegral x) + 1)) `mod'` 6 == 0


-- For efficiency, we check triangle numbers for pentagonality
-- and for hexagonality by checking if there's an integer solution
-- for the quadratics that generated penta- / hexagonal numbers.
-- This makes it run basically instantly :).
p045 :: Int
p045 = head 
     . filter isPenta
     . filter isHexa
     $ map triangle [286..]
  where
    triangle n = n * (n + 1) `div` 2 
    isPenta x  = (1 + sqrt (24 * (fromIntegral x) + 1)) `mod'` 6 == 0
    isHexa  x  = (1 + sqrt (8 * (fromIntegral x) + 1)) `mod'` 4 == 0


-- For every odd composite, we enumerate all possible squares and
-- check whether the required difference is prime. Runs basically
-- instantly, so certainly good enough :).
p046 = head . dropWhile test . filter composite $ [9,11..]
  where
    composite i = any ((==0) . (i `rem`)) [2..isqrt i]
    prime       = not . composite
    isqrt       = truncate . sqrt . fromIntegral
    test i      = not . null $ [1 | s     <- map (^2) [1..isqrt i],
                                    let p =  i - 2 * s,
                                    prime p]


-- Straightfoward direct search without any cleverness. Runs in a few seconds.
p047 = head $ filter test [2..]
  where
    test n =  all ((==4) . length) factors
           && all null [(factors !! i) `intersect` (factors !! j) | i <- [0..3],
                                                                    j <- [0..3],
                                                                    i < j]
      where
        factors = map primeFactors' [n, n + 1, n + 2, n + 3]
        primeFactors' = map (\l -> (head l) ^ (length l))
                      . group
                      . primeFactors


-- Direct calculation
p048 = (read :: String -> Int) . reverse . take 10 . reverse . show
     . sum . map (\n -> n^n) . enumFromTo 1


-- Direct search with lots of constraints for early stopping.
-- Runs basically instantly.
p049 = (\(p1, p2, p3) -> p3 + 10000 * p2 + 100000000 * p1)
     . head
     $ [(p1, p2, p3) | p1 <- primes,
                       p1 /= 1487,
                       p2 <- primes,
                       p2 > p1,
                       isPerm p1 p2,
                       let p3 = p2 + (p2 - p1),
                       p3 `elem` primes,
                       isPerm p1 p3]
  where
    primes     = filter isPrime [1000..9999]
    isPerm i j = (sort (show i)) == (sort (show j))


-- We start at every individual prime and construct a sublist from it
-- to the end of the list of all primes. Then, every sublist is shrunken
-- down from the right until its sum reaches another prime number for
-- the first time. Then, we return the length of the sublist and the
-- prime it sums to.
-- Two optimisations make this run acceptably fast:
-- First, while shrinking a sublist, we keep track of the current sum,
-- so we don't have to recalculate the sum from scratch at every step.
-- Second, we reverse a sublist before attempting to shrink it. In this
-- way, we have to remove the head, instead of the last element of the
-- list, which is significantly faster in Haskell.
-- These two reduce the runtime to 1:46, which is not perfect, but okay.
-- (Without optimisations, I had to abort after 15min.)
p050 n = snd
       . maximumBy (comparing fst)
       $ map (\l -> shrink (sum l) (reverse l)) listtails
  where
    primes       = filter isPrime [2..n-1]
    listtails    = reverse . init . tails $ primes
    shrink suml l@(x:xs)
        |suml > n              = shrink (suml - x) xs
        |(sum l) `elem` primes = (length l, sum l)
        |otherwise             = shrink (suml - x) xs


main = do
    input042 <- readFile "0042_words.txt"

    --print $ "Problem 041: " ++ show (p041)
    --print $ "Problem 042: " ++ show (p042 input042)
    --print $ "Problem 043: " ++ show p043
    --print $ "Problem 044: " ++ show p044
    --print $ "Problem 045: " ++ show p045
    --print $ "Problem 046: " ++ show p046
    --print $ "Problem 047: " ++ show p047
    --print $ "Problem 048: " ++ show (p048 1000)
    --print $ "Problem 049: " ++ show p049
    print $ "Problem 050: " ++ show (p050 1_000_000)

    print $ "---------- Done. ----------"