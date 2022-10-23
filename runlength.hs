{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
import GHC.Unicode

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0 ..], x == x']
-- returns list of positions of element x in list xs

count_sucessvly :: (Num a, Ord a) => [a] -> a -> a
count_sucessvly [] n = n
count_sucessvly (x:xs) n
    | x == n = count_sucessvly  xs (n+1)
    | x>n = n
    | otherwise = 0
--counts how many times one letter repeats successively.

encode :: String -> String
encode [] = []
encode text = if repeating >1 then show repeating ++ head text:encode (drop repeating text) else head text:encode (drop repeating text)
    where repeating = count_sucessvly (positions (head text) text) 0
-- head code is 'a' and show repeating is 'b':[]. head code is a pure char and show repeating is a list with one entry that is a char, a string with only one element.
-- if statement is to remove 1 when repeating is 1.

untilnotDigit :: [Char] -> [Char]
untilnotDigit [] = []
untilnotDigit (x:xs) = if isDigit x then x:untilnotDigit xs else []
-- gets digits until the next char is no longer a digit.


decode :: String -> String
decode [] = []
decode encodedText = replicate repeating (head $ drop length_digit encodedText) ++ decode (tail $ drop length_digit encodedText)
    where repeating = if null (untilnotDigit encodedText) then 1 else read (untilnotDigit encodedText)
          length_digit = length (untilnotDigit encodedText)



main :: IO ()
main = do
{- comentario multiline
-}
--comentario oneline
-- compilar  ghc -o hw hw.hs
--correr ./h`
-- ctrl + space run in VSa
-- ctr + S to save in VS 



let code = "XYZ"
let dcode= encode code
let repeating = untilnotDigit dcode
print(encode code)
print (dcode)
print(repeating)
print(decode (dcode))
--print(rmdups $ test code)
--print(test2 code)
--print(func (test2 code) 0 )