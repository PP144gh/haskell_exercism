import Data.Char (toUpper)

scoreLetter :: Char -> Integer
scoreLetter letter
    | any (==letterC) "AEIOULNRST" = 1
    | elem letterC "DG" = 2
    | letterC `elem` "BCMP" = 3
    | any (==letterC) "FHVWY" = 4
    | any (==letterC) "K" = 5
    | any (==letterC) "JX" = 8
    | any (==letterC) "QZ" = 10
    | otherwise = 0
       where letterC = toUpper letter

scoreWord :: String -> Integer
scoreWord word = sum $ map scoreLetter word
--scoreWord = sum . map scoreLetter








main :: IO ()
main = do
{- comentario multiline
-}
--comentario oneline
-- compilar  ghc -o hw hw.hs
--correr ./hw
-- ctrl + space run in VS
-- ctr + S to save in VS 




let num=3
print(scoreLetter 'Z')
print(scoreWord "Ola")