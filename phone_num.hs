
import Data.Char 
import Data.List
import Data.Maybe
import Data.Foldable



number :: String -> Maybe String
number [] = Nothing
number xs 
    | head ns == '1' = number (drop 1 ns)
    | head ns == '0' || head ns == '1' || ns !! 3 == '0' ||  ns !! 3 == '1' = Nothing 
    | length ns == 10 = Just ns
    | otherwise = Nothing
    where ns= filter (\x -> x `elem` goodchars) xs
          goodchars="0123456789"
--improvement of previous version.
    --where ns= filter (\x -> x `notElem` badchars) (map toLower xs)
          --badchars="abcdefghijklmnopqrstuvxywz" ++ ['(',')','-','.',',',' ','@',':','!','?','+',';','_','*']



main :: IO ()
main = do
{- comentario multiline
-}
--comentario oneline
-- compilar  ghc -o hw hw.hs
--correr ./hw
-- ctrl + space run in VS
-- ctr + S to save in VS 



let num ="223 456   7890   " 
let goodchars="123456789"
let ns= filter (\x -> x `elem` goodchars) (map toLower num)
let badchars="abcdefghijklmnopqrstuvxywz" ++ ['(',')','-','.',',',' ','@',':','!','?','+',';','_','*']

let ns2 = filter (\x -> x `notElem` badchars) (map toLower num)

print(ns)
print(ns2)

print(number num)