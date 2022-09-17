-- SUM OF MULTIPLES --

--defines infinite integer list that start in a and each element is the previous one -1
listdef :: Integer -> [Integer]
listdef a = a : listdef (a-1)

-- checks if limit is divisible by the elements of factors. if it is, return the number, otherwise keep trying until you run out of factors.
helper :: [Integer] -> Integer -> Integer 
helper _ 0 = 0
helper [] _ = 0
helper (f:factors) limit 
    | f /= 0 && limit `mod` f == 0=  limit 
    | otherwise = helper factors limit 



sumOfMultiples:: [Integer] -> Integer -> Integer 
sumOfMultiples factors limit = sum (tail tosum)
                               where list = take (fromIntegral limit) (listdef limit)
                                     tosum = map (helper factors) list

--list is defined as taking the first limit elements of the infinite list previously defined.
-- to sum is a list which has 0's or a number that belongs to list. if it an entry is
-- non zero its because it is divisible by one of the factors in the factors list.
-- tail of to sum is to remove the original number limit. even if it is divisible
-- by factors we dont want to sum over it "up to but not including that number."



------------------------
