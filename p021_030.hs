{-# LANGUAGE NumericUnderscores #-}

import Control.Arrow ((>>>))
import Data.Char (ord, digitToInt)
import Data.List (sort, (\\))
import Data.Set (Set)
import qualified Data.Set as Set
import Utils (tok)

-- Brute-force with obvious constraints.
-- Proper divisors via trial division -- naive,
-- but still fast enough.
p021 i = sum . concatMap (\(a, b) -> [a, b]) $ pairs
  where
    pairs = [(a, b) | a <- [1..i - 1],
                      let b = d a,
                      d b == a,
                      a > b]
    d i   = sum . filter ((i `mod`) >>> (==0)) $ [1..i `div` 2]


-- Direct calculation.
p022 = sum . map value . zip [1..] . sort . tok ['"', ',']
  where
    value (i, xs) = i * (sum $ map (\c -> ord c - 64) xs)


-- Reasonably fast thanks to set lookups.
p023 = sum . filter (isNotSum) $ [1..28123]
  where
    isNotSum i = any (\a -> (i - a) `Set.member` abuSet) abundants
    abuSet     = Set.fromList abundants
    abundants  = filter (\n -> n < d n) [1..28123]
    d i        = sum . filter ((i `mod`) >>> (==0)) $ [1..i `div` 2]


-- Neat recursive solution to avoid brute force :).
p024' i = go "0123456789" (i - 1)
  where
    fac      = foldl1 (*) . enumFromTo 1
    go [c] _ = [c]
    go cs  i = c' : go cs' i'
      where
        f   = fac $ (length cs - 1)   -- There are f blocks,
        idx = i `div` f               -- and we are in the idx-th one.
        c'  = cs !! idx               -- Thus, the idx-th char is the next one.
        cs' = cs \\ [c']              -- And these are the remaining ones.
        i'  = i - f * idx             -- Updated index for the next round.


-- Using the fastest native memoised version of fib I know.
-- This runs basically instantly! :D
p025 d = (+1) . length . takeWhile (< 10^(d - 1)) $ fib
  where
    fib = 1 : 1 : zipWith (+) fib (tail fib)


-- TBD
p026 = undefined


-- TBD
p027 = undefined


-- Generating ever circular layer and picking the diagonal numbers.
-- Runs basically instantly, so further optimisation is not required.
-- Alternatively, we could also come up with closed-form solutions
-- for the diagonal elements.
p028 n
  |n < 3  = error "Not implemented."
  |even n = error "Not meaningful."
  |otherwise = (+1) . sum . concatMap layer $ [3,5..n]
  where
    layer i = map (numbers!!) $ [counts `div` 4 - 1,
                                 counts `div` 2 - 1 ,
                                 3 * counts `div` 4 - 1,
                                 counts - 1]
      where
        numbers = [((i - 2) ^ 2 + 1)..i^2]
        counts  = length numbers


-- Simple enumeration with removal of duplicates via set.
-- Runs basically instantly, so good enough :).
p029 i = Set.size $ Set.fromList [a^b | a <- [2..i], b <- [2..i]] 


-- We don't care about trivial sums, i.e. we want at least 2 digits,
-- thus we begin with 10^1.
-- Since 7 x 9^5 = 413'343 < 10^7, we do not have to consider any
-- number with seven digits or more. Thus, we use 10^6 as upper bound.
-- The rest is just an exhaustive check.
p030 = sum . filter fulfils $ [10..1_000_000]
  where
    fulfils i = i == (sum $ map ((^5) . digitToInt) $ show i)


main = do
    input022 <- readFile "0022_names.txt"


    -- print $ "Problem 021: " ++ show (p021 10_000)
    -- print $ "Problem 022: " ++ show (p022 input022)
    -- print $ "Problem 023: " ++ show p023
    -- print $ "Problem 024: " ++ (p024' 1_000_000)
    -- print $ "Problem 025: " ++ show (p025 1000)
    

    -- print $ "Problem 028: " ++ show (p028 1001)
    -- print $ "Problem 029: " ++ show (p029 100)
    -- print $ "Problem 030: " ++ show p030

    print $ "---------- Done. ----------"