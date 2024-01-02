{-# LANGUAGE NumericUnderscores #-}

import Data.Bits (xor)
import Data.Char (chr, digitToInt, ord)
import Data.List (isInfixOf, sort)
import Data.Ratio
import Utils (isPrime, readInt, readInteger, tok)


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


-- Direct search. Reasonably fast.
p058 = (+2) 
     . fst
     . last
     . takeWhile (\(i, f) -> f > 1 % 10)
     . drop 1 
     . zip [1,3..]
     . map (\(p, l) -> p % l)
     . scanl1 (\(p1, l1) (p2, l2) -> (p1 + p2, l1 + l2))
     . map (\xs -> (primeCount xs, length xs))
     $ map (diagsLayer) [1,3..]
  where
    primeCount xs = length $ filter isPrime xs

    diagsLayer 1 = [1]
    diagsLayer n
        |even n    = error "Not meaningful."
        |otherwise = map (l!!) [length l `div` 4 - 1,
                                length l `div` 2 - 1,
                                3 * length l `div` 4 - 1,
                                length l - 1]
          where
            l = [(n - 2) ^ 2 + 1 .. n ^ 2]


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


-- W.l.o.g. we assume p1 < p2 < p3 < p4 < p5. The following code
-- may seem utterly ugly, however the style is deliberate to
-- provide lots of opportunities for "early stopping". This
-- approach finishes in approximately one second, whereas a
-- more elegant version (where we generate all distinct pairs
-- of elements of ps and check whether they all concatenate to
-- a prime number) would not stop within approx. 10 minutes.
p060 = head $ [sump | p1 <- primes,
                      p2 <- filter (>p1) primes,
                      test p1 p2,
                      p3 <- filter (>p2) primes,
                      test p1 p3, test p2 p3,
                      p4 <- filter (>p3) primes,
                      test p1 p4, test p2 p4, test p3 p4,
                      p5 <- filter (>p4) primes,
                      test p1 p5, test p2 p5, test p3 p5, test p4 p5,
                      let sump = p1 + p2 + p3 + p4 + p5]
  where
    test i j = isPrime (conc i j) && isPrime (conc j i)
    conc i j = readInt $ (show i) ++ (show j)
    primes   = filter isPrime [2..10000]


main = do
    input059 <- readFile "0059_cipher.txt"

    print $ "Problem 052: " ++ show p052
    print $ "Problem 053: " ++ show (p053 1_000_000)

    print $ "Problem 055: " ++ show p055
    print $ "Problem 056: " ++ show (p056 99)

    print $ "Problem 058: " ++ show p058
    print $ "Problem 059: " ++ show (p059 input059)
    print $ "Problem 060: " ++ show p060

    print $ "---------- Done. ----------"