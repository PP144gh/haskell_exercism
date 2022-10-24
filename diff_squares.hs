difference :: Integral a => a -> a
difference n= squareOfSum n - sumOfSquares n 

squareOfSum :: Integral a => a -> a
squareOfSum n = (foldr (+) 0 [1..n])^2
--squareOfSum n= (sum [1..n])^2

sumOfSquares :: Integral a => a -> a
sumOfSquares n = foldr ((+) . (^2)) 0 [1..n]
--sumOfSquares n = sum $ map ((^2)) [1..n]




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
print(sumOfSquares num)
print(squareOfSum num)
print(difference num)