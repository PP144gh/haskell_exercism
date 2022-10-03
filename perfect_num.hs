---- Perfect Numbers -----

import Data.List (unfoldr)

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

-- data declaration. constructors: Deficient, Perfect and Abundant. Classificiation is an instnace of the Eq and Show class. show function works on Classification. also == and != are defined.

classify :: Int -> Maybe Classification
classify num
    | num<=0 = Nothing
    | otherwise = if num == aliquotsum num then Just Perfect else
        if num < aliquotsum num then Just Abundant else 
            if num>aliquotsum num then Just Deficient else Nothing

   



aliquotsum :: Int -> Int
aliquotsum num = sum (filter (0 /=) (tail (unfoldr (\x -> if  x == 0 then Nothing else Just (f num x, x-1)) num)))
                 where f num x = if num `mod` x == 0 then x else 0
  
aliquotsum' :: Int -> Int
aliquotsum' num = sum (tail (reverse ([x | x <- [1..num], num `mod` x == 0])))

--another way after learning list comprehension. chap 5 of hutton's book. tail is faster tham removing the last element of the list, hence the reverse. this list starts at 1 and goes until num.
-- previous way of doing it started at num and went until 1.
----------------------------------------


main :: IO ()
main = do
{- comentario multiline
-}
--comentario oneline
-- compilar  ghc -o hw hw.hs
--correr ./hw
-- ctrl + space run in VS
-- ctr + S to save in VS 




let num=12
print (aliquotsum num)
print(aliquotsum' num)
print(classify num)

