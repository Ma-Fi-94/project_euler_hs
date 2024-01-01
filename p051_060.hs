{-# LANGUAGE NumericUnderscores #-}

import Data.Bits (xor)
import Data.Char (chr, digitToInt, ord)
import Data.List (isInfixOf, sort)
import Utils (readInt, readInteger, tok)


-- Trivial direct search, runs close to instantly.
p052 = head $ [x | x <- [1..],
                   all (\i -> isPermutation x (i*x)) [2..6]]
  where
    isPermutation i j = (sort (show i)) == (sort (show j))


-- Trivial direct search, runs close to instantly.
p053 i = length . filter (>i) $ [cnk n k | n <- [1..100],
                                           k <- [1..n]]
  where
    cnk n k = (fac n) / ((fac k) * (fac (n-k))) 
    fac 0   = 1
    fac i   = foldl1 (*) [1..i]


-- Trivial direct search, runs close to instantly.
p055 = length $ filter isLychrel [1..9999]
  where
    revadd :: Integer -> Integer
    revadd i    = i + (readInteger (reverse (show i)))

    isPali :: Integer -> Bool
    isPali i    = (show i) == (reverse (show i))

    isLychrel :: Integer -> Bool
    isLychrel i = go 1 (revadd i)
      where
        go :: Int -> Integer -> Bool
        go 50 _ = True
        go c i
            |isPali i  = False
            |otherwise = go (c + 1) (revadd i) 


-- Exhaustive search is still very fast.
p056 i = maximum [digitSum (a ^ b) | a <- [1..i],
                                     b <- [1..i]]
  where
    digitSum = sum . map digitToInt . show


-- Exhaustive search through the space of keys and checking for
-- the occurence of some common English words.
p059 input = sum
           . map ord
           . head 
           $ [plain | k1 <- [97..122],
                      k2 <- [97..122],
                      k3 <- [97..122],
                      let plain = decode [k1, k2, k3],
                      all (`isInfixOf` plain) ["the", "and", "then"]]
  where
    cipherInt  = map readInt . tok "," $ input
    decode key = map chr $ zipWith xor cipherInt repeatedKey
      where
        repeatedKey = concat . repeat $ key


main = do
    input059 <- readFile "0059_cipher.txt"

    print $ "Problem 052: " ++ show p052
    print $ "Problem 053: " ++ show (p053 1_000_000)

    print $ "Problem 055: " ++ show p055
    print $ "Problem 056: " ++ show (p056 99)

    print $ "Problem 059: " ++ show (p059 input059)

    print $ "---------- Done. ----------"