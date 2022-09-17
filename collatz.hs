-- COLLATZ CONJECTURE --

--helper functions have an argument that keeps track 
-- of the number of iterations


collatzhelper ::  Integer -> Integer -> Integer
collatzhelper n k 
    | ((n > 1) && (collatzcalc n == 1)) = k+1
    | ((n>1) && (collatzcalc n /= 1)) = collatzhelper (collatzcalc n) k+1
    | n <=1 = 0


collatzcalc :: Integer -> Integer
collatzcalc n = if (mod n 2 == 0)
            then div n 2 
            else 3*n+1
    
collatz :: Integer -> Maybe Integer
collatz n 
    | n<1 = Nothing
    | n>= 1 = Just (collatzhelper n 0)

  
collatz :: Integer -> Maybe Integer
collatz n | n <= 0 = Nothing
          | n == 1 = Just 0
          | even n     = succ <$> collatz (n `div` 2)
          | otherwise  = succ <$> collatz (3 * n + 1)

-- <$> : Functor f => (a -> b) -> f a -> f b
-- succ <$> ??


--Converts Just a to a && Nothing to -1 (pattern matching)
justToIntOnly :: (Integral a) => Maybe a -> a
justToIntOnly (Just a) = a
justToIntOnly Nothing  = -1

collatzcalc :: Integer -> Integer
collatzcalc n = if even n
            then div n 2 
            else 3*n+1
    
collatz :: Integer -> Maybe Integer
collatz 1 = Just 0
collatz n
    | n >=1 = Just (1 + justToIntOnly (collatz (collatzcalc n)))
    | otherwise  = Nothing

--------------------------
