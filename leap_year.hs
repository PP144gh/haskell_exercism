-- LEAP YEAR --

{-
isLeapYear :: Integer -> Bool
isLeapYear year 
  | (mod year 4 == 0) && (mod year 100 /= 0 || mod year 400 == 0) = True
  | otherwise = False

-}

isLeapYear :: Integer -> Bool
isLeapYear year = if ( (mod year 4 == 0) && (mod year 100 /= 0 || mod year 400 == 0)) then True else False

-----------------
