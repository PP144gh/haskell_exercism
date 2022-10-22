decode :: String -> String
decode encodedText = replicate repeating (head (tail encodedText))
    where repeating = read [head encodedText]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0 ..], x == x']


fil :: (Num a, Ord a) => [a] -> a -> a
fil [] n = n
fil (x:xs) n
    | x == n = fil xs (n+1)
    | x>n = n
    | otherwise = 0
--counts how many times one letter repeats successively.

encode :: String -> String
encode [] = []
encode text = show repeating ++ head text:encode (drop repeating text)
    where repeating = fil (positions (head text) text) 0
-- head code is 'a' and show repeating is 'b':[]. head code is a pure char and show repeating is a list with one entry that is a char, a string with only one element.
main :: IO ()
main = do
{- comentario multiline
-}
--comentario oneline
-- compilar  ghc -o hw hw.hs
--correr ./h`
-- ctrl + space run in VSa
-- ctr + S to save in VS 



let code = "WWWWWWWWWWWWBBZZZW"
let dcode= encode code
print(encode code)
print (dcode)
print(decode dcode)
--print(rmdups $ test code)
--print(test2 code)
--print(func (test2 code) 0 )