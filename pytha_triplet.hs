-- PYTHAGOREAN TRIPLET --


--list of tuples 
tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum summ = reverse (search (div summ 3 -1) (summ - div summ 3 + 1 ))
-- no point in starting at sumbc=0 and as=summ  , because sumbc (b+c) has to be bigger than summ/3, otherwise impossible
-- to satisfy a< b && b<c . example: for summ=100 minimum possible is 32 33 35 



oneList :: Int -> Int -> [Int] -> [(Int, Int,Int)]
oneList 0 _ _ = []
oneList _ _ [] = []
oneList _ 0 _ = []
oneList a sumbc (b:bs) = if  a*a + b*b == c*c
                        then (a, b, c) : oneList a sumbc bs
                        else oneList a sumbc bs
                        where c =  sumbc - b
                             
--enforces a*a + b*b == c*c


search :: Int  -> Int -> [(Int,Int,Int)]
search 0 _ = []
search _ 0 = []
search as  sumbc = list3 ++ search (as-1) (sumbc+1)
                 where list = [n .. maxn]
                       list3 = oneList as sumbc list
                       n= as + 1 
                       maxn = div sumbc 2
-- list has the possible values for b such that a<b and b>c are satisfied

-- My first solution for this problem was way slower. I listed all possible combinanations of the triplets that
-- respected a+b+c=n and then applied logic to get the subset that satisfied the other conditions.
-- Its way faster if I understand by hand what are the possible limits for a b c that have a+b+c=n such that a<b<c and only search in that subset, applying logic to find out which ones
-- have a**2 + b**2 = c**2

--Or more compact version
{-
tripletsWithSum :: Int -> [(Int,Int,Int)]
tripletsWithSum n =
    filter (\(a,b,c) -> a*a+b*b == c*c)
    $ triplets n

    
triplets :: Int -> [(Int,Int,Int)]
triplets n = [
  (a, b, c)
    | a <- [1..n`div`3]
    , b <-[a..(n-a)`div`2]
    , c <- [n-a-b]
  ]

-}
  ------------------------------------------------------------------

