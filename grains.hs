--- GRAINS ------


square :: Integer -> Maybe Integer
square 0 = Nothing
square n 
    | n<0 = Nothing
    | n>64 = Nothing
    | otherwise = Just (2^(n-1))

converter :: Maybe Integer -> Integer
converter x = case x of
            Just i -> i
            Nothing -> 0

total :: Integer
total = sum (map converter (map square lst))
      where lst = [1 .. 64]
      
            
-------------------------------------------------
