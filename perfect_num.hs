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
aliquotsum num = sum (filter (0 /=) (drop 1 (unfoldr (\x -> if  x == 0 then Nothing else Just (f num x, x-1)) num)))
                 where f num x = if num `mod` x == 0 then x else 0
  



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




let num=6
print (aliquotsum num)
print(classify num)

