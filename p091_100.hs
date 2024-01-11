{-# LANGUAGE NumericUnderscores #-}

import Data.Function (on)
import Data.List (maximumBy, nubBy)
import Utils (tok)


-- This is the most inefficient code conceivable to mankind,
-- however I utterly failed at getting the right answer using
-- any more efficient approachwa at half past 3 in the morning.
-- Compiled for efficiency, it still runs within 3 seconds,
-- so I am willing to take it (for now).
p091 n = length
       . nubBy triEq
       $ [((x1, y1), (x2, y2)) | x1 <- [0..n],
                                 y1 <- [0..n],
                                 x2 <- [0..n],
                                 y2 <- [0..n],
                                 let a2 = x1^2 + y1^2,
                                 let b2 = x2^2 + y2^2,
                                 let c2 = (y2 - y1) ^ 2 + (x2 - x1) ^ 2,
                                 a2 > 0, b2 > 0, c2 > 0,
                                 a2 + b2 == c2 || a2 + c2 == b2 || b2 + c2 == a2]
  where
     triEq ((x1, y1), (x2, y2)) ((x1', y1'), (x2', y2')) =
               (x1, y1) == (x1', y1') && (x2, y2) == (x2', y2')
            || (x1, y1) == (x2', y2') && (x2, y2) == (x1', y1')


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