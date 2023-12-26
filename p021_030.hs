{-# LANGUAGE NumericUnderscores #-}

import Control.Arrow ((>>>))
import Data.Char (ord)
import Data.List (sort)
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


-- TBD
p023 = undefined


-- TBD
p024 = undefined


-- Using the fastest native memoised version of fib I know.
-- This runs basically instantly! :D
p025 d = (+1) . length . takeWhile (< 10^(d - 1)) $ fib
  where
    fib = 1 : 1 : zipWith (+) fib (tail fib)


-- TBD
p026 = undefined


-- TBD
p027 = undefined


-- TBD
p028 = undefined


-- TBD
p029 = undefined


-- TBD
p030 = undefined


main = do
    input022 <- readFile "0022_names.txt"


    -- print $ "Problem 021: " ++ show (p021 10_000)
    -- print $ "Problem 022: " ++ show (p022 input022)


    print $ "Problem 025: " ++ show (p025 1000)

    print $ "---------- Done. ----------"