{-# LANGUAGE NumericUnderscores #-}

import Data.Function (on)
import Data.List (maximumBy)
import Utils (tok)

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

    print $ "Problem 097: " ++ show p097
    print $ "Problem 099: " ++ (show $ p099 input099)

    print $ "---------- Done. ----------"