

handshake :: Int -> [String]
handshake 0 = []
handshake n = if length bin > 4 && bin !! 4 == 1 then reverse (helper bin 0) else helper bin 0
              where bin = toBinary n



helper :: [Int] -> Int -> [String]
--helper bin len = reverse (helper bin (len-1))
helper [] _ =[]
helper bin k 
    | last bin == 1 = if k==0 then "wink":helper (init bin) (k+1) else
        if k==1 then "double blink":helper (init bin) (k+1) else
            if k==2 then "close your eyes":helper (init bin) (k+1) else
                if k==3 then "jump":helper (init bin) (k+1) else helper (init bin) (k+1)
    | otherwise = helper (init bin) (k+1)
 -- k is the exponent of 2^k. for two, 10, k=0, refers to the 0 and k=1 refers to the 1.


toBinary :: Int -> [Int]
toBinary 1 = [1]
toBinary 0 = [0]
toBinary n  
    | even n = toBinary (n `div` 2) ++ [0]
    | otherwise = toBinary (n `div` 2) ++ [1]

-- divide (integer division) the number by 2 and add 0 every time the result is even and one every time the result is odd


main :: IO ()
main = do
{- comentario multiline
-}
--comentario oneline
-- compilar  ghc -o hw hw.hs
--correr ./hw
-- ctrl + space run in VS
-- ctr + S to save in VS 


let n=31
let bin = toBinary n
let len = length bin
print(bin)
--print("wink":helper [1] 1)
print(handshake n)

