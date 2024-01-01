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
p056 = maximum [digitSum (a ^ b) | a <- [1..99],
                                   b <- [1..99]]
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

    print $ "Problem 055: " ++ show p055
    print $ "Problem 056: " ++ show p056

    print $ "Problem 059: " ++ show (p059 input059)

    print $ "---------- Done. ----------"