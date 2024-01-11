{-# LANGUAGE NumericUnderscores #-}

import Data.Function (on)
import Data.List (maximumBy, nub)
import Utils (tok)


-- Slightly more optimised version of a previous, very naive approach.
-- In particular, element-wise tuple comparison frees us from a
-- costly 'nubBy'. Also dropped check for c^2 > 0, without speedup
-- though. Runs in less than half a second, compared to 2.5 seconds before.
p091 n = length $ [1 | x1 <- [0..n],
                       y1 <- [0..n],
                       x2 <- [0..n],
                       y2 <- [0..n],
                       (x1, y1) < (x2, y2),
                       let a2 = x1^2 + y1^2, a2 > 0,
                       let b2 = x2^2 + y2^2, b2 > 0,
                       let c2 = (y2 - y1) ^ 2 + (x2 - x1) ^ 2,
                       a2 + b2 == c2 || a2 + c2 == b2 || b2 + c2 == a2]


-- It is, what it is.
p097 = (read :: String -> Int)
     . reverse . take 10 . reverse . show
     $ 28433 * 2^7830457 + 1


-- Direct calculation, using the fact that we can compare log-ed values.
-- This frees us from computing and comparing very long numbers.
p099 = fst
     . maximumBy (compare `on` snd)
     . zip [1..]
     . map (\l -> (fromIntegral (l!!1)) * log (fromIntegral (l!!0)))
     . map (map (read :: String -> Integer))
     . map (tok ",")
     . lines


main = do
    input099 <- readFile "0099_base_exp.txt"
    print $ "Problem 091: " ++ (show $ p091 50)

    print $ "Problem 097: " ++ show p097
    print $ "Problem 099: " ++ (show $ p099 input099)

    print $ "---------- Done. ----------"