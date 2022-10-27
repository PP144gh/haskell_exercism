-- FACTORIAL --


factorial :: Integer ->  Integer
factorial 0 = 1
--factorial n = n * factorial (n-1)
factorial n = foldr (*) 1 [n,n-1..1]
--factorial n = product [n,n-1..1]

---------------------

