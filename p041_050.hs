{-# LANGUAGE NumericUnderscores #-}

import Data.Char (ord)
import Data.Fixed (mod')
import Data.List (inits, permutations, sort)
import Utils (tok, isPrime, readInt)


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

 
-- Direct calculation
p048 = (read :: String -> Int) . reverse . take 10 . reverse . show
     . sum . map (\n -> n^n) . enumFromTo 1


main = do
    input042 <- readFile "0042_words.txt"

    --print $ "Problem 041: " ++ show (p041)
    --print $ "Problem 042: " ++ show (p042 input042)
    --print $ "Problem 043: " ++ show p043
    print $ show p044
    --print $ "Problem 045: " ++ show p045
    --print $ "Problem 046: " ++ show p046

    --print $ "Problem 048: " ++ show (p048 1000)
    
    print $ "---------- Done. ----------"