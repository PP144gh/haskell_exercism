import Data.Char (digitToInt)
import Data.Char (isDigit)


isbn :: String -> Bool
isbn word = (length code == 10) && (calculator code 10 `mod` 11 == 0)
      where code = filter (/= '-') word


calculator :: [Char] -> Int -> Int
calculator [] _ = 0
calculator ['X'] 1 = 10
calculator code n
    | isDigit $ head code = digitToInt (head code) * n + calculator (tail code) (n-1)
    | otherwise = 1 -- guarantees False in isbn. 1 mod 11 /= 0


test :: [Char] -> [Char]
test code = filter (/= '-') code


main :: IO ()
main = do
{- comentario multiline
-}
--comentario oneline
-- compilar  ghc -o hw hw.hs
--correr ./h`
-- ctrl + space run in VS
-- ctr + S to save in VS 




let code = "3-598-21507-X"
print(test code)
print(length $ test code)
print((calculator (test code) 10 `mod` 11) == 0 )
print(isbn code)
