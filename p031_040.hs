{-# LANGUAGE NumericUnderscores #-}


-- Just counting the number of possibilities, and looking at
-- every coin only once (in descending order) makes this run
-- nearly instantly. (I first tried a naive brute-force /
-- enumerative approach without any success.)
p031 :: Int -> Int
p031 = go [200, 100, 50, 20, 10, 5, 2, 1]
  where
    go _ 0  = 1
    go [] _ = 0
    go (c:cs) rest = sum $ map (\i -> go cs (rest - i * c)) choices
      where
        choices = [0..(rest `div` c)]



main = do
    print $ "Problem 031: " ++ show (p031 200)

    print $ "---------- Done. ----------"