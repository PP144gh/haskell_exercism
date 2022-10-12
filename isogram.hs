import qualified Data.Char
import Data.Char (toLower)

isIsogram :: String -> Bool
isIsogram word = not . any (>1) $ [count x word2 | x <- word2 ]
     where word2 = map toLower word


count :: Char -> [Char] -> Int
count x xs = length [x' | x' <- xs, x == x' && x /= ' ' && x /= '-']
-- counts how many x chars appear in xs. ignores white spaces and -


test :: [Char] -> [Int]
test word2= [count x word2 | x <- word2 ]

main :: IO ()
main = do
{- comentario multiline
-}
--comentario oneline
-- compilar  ghc -o hw hw.hs
--correr ./hw
-- ctrl + space run in VS
-- ctr + S to save in VS 

--let tup = zip [0 | x <- ['a'..'z']] ['a'..'z']
let word = "ola-adeus"
print(isIsogram word)
print(test word)
print(count '-' word)