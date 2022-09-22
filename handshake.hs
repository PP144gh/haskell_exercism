import Distribution.Utils.MD5 (binaryGetMD5)


handshake :: Int -> [String]
handshake 0 = []
handshake n = if length bin > 4 && bin !! 4 == 1 then reverse (helper bin 0) else helper bin 0
              where bin = toBinary n



helper :: [Int] -> Int -> [String]
helper [] _ =[]
helper bin k 
    | k > 4 = helper [] 0 -- stop after the fifth digit (fifth because it started from 0), as it will not change the return
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



-- very ingenious way of doing this without converting the number to binary

{-
handshakeAcc :: [String] -> Int -> [String]
handshakeAcc acc i
    | n >= 16 = reverse (handshakeAcc acc (n-16))
    | n >= 8 = handshakeAcc ("jump":acc) (n-8)
    | n >= 4 = handshakeAcc ("close your eyes":acc) (n-4)
    | n >= 2 = handshakeAcc ("double blink":acc) (n-2)
    | n == 1 = handshakeAcc ("wink":acc) (n-1)
    | n == 0 = acc
    where n = i `mod` 32

 -- this mod 32 has the same effect as my k> 4 = helper [] 0. stops doing more things than necessary. rest is the same idea but without thinking in binary and using the exponent k of 2^k

handshake :: Int -> [String]
handshake = handshakeAcc []

-}

main :: IO ()
main = do
{- comentario multiline
-}
--comentario oneline
-- compilar  ghc -o hw hw.hs
--correr ./hw
-- ctrl + space run in VS
-- ctr + S to save in VS 


let n=1023
let bin = toBinary n
let len = length bin
print(bin)
--print("wink":helper [1] 1)
print(handshake n)

