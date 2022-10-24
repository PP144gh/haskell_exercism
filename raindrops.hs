convert :: Int -> String
convert n = if null sound then show n else sound 
    --where sound = magic n 3 (magic n 5 (magic n 7 []))
    where sound = magic n 3 . magic n 5 . magic n 7 $ []

magic :: Integral a => a -> a -> [Char] -> [Char]
magic n num list
    | n `mod` num == 0 = add ++ list
    | otherwise = list
    where add
            | num == 3 = "Pling"
            | num == 5 = "Plang"
            | num == 7 = "Plong"
            | otherwise = []


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
print(convert 34)