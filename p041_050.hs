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

    print $ "Problem 042: " ++ show (p042 input042)

    print $ "Problem 046: " ++ show p046

    print $ "Problem 048: " ++ show (p048 1000)
    
    print $ "---------- Done. ----------"