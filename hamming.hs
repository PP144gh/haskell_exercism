
distance :: String -> String -> Maybe Int
distance [] [] = Just 0
distance xs ys = if length xs /= length ys then Nothing else Just (count xs ys 0)


count :: String -> String -> Int -> Int
count [] _ n = n
count _ [] n = n
count (x:xs) (y:ys) n
    | x /= y = count  xs ys (n+1)
    | otherwise = count xs ys n



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
print(distance "A" "T")