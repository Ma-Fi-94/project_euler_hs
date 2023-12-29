{-# LANGUAGE NumericUnderscores #-}

import Data.Char (ord)
import Utils (tok)

-- The maximum word score is 192, so we just generate the first 200
-- triangle numbers and check how many words have a score that's
-- element of that list. An obious improvement would be to use
-- the quadratic formula to check whether a score is a triangle number.
-- However, given the tiny search space this is not required at all.
p042 = length . filter (`elem` triangles) . map wordScore . tok ['"', ',']
  where
    wordScore = sum . map ((subtract 64) . ord)
    triangles = map (\n -> n * (n + 1) `div` 2) [1..200]


-- Direct calculation
p048 = (read :: String -> Int) . reverse . take 10 . reverse . show
     . sum . map (\n -> n^n) . enumFromTo 1


main = do
    input042 <- readFile "0042_words.txt"

    print $ "Problem 042: " ++  show (p042 input042)

    print $ "Problem 048: " ++  show (p048 1000)
    
    print $ "---------- Done. ----------"