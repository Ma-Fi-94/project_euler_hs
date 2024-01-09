{-# LANGUAGE NumericUnderscores #-}


-- It is, what it is.
p097 = reverse . take 10 . reverse . show $ 28433 * 2^7830457 + 1

main = do

    print $ "Problem 097: " ++ show p097

    print $ "---------- Done. ----------"