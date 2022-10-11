



armstrong :: Integral a => a -> Bool
armstrong num = foldr (((+)) . (^l)) 0 list == num
    where list = digs num
          l = length list






digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]





main :: IO ()
main = do
{- comentario multiline
-}
--comentario oneline
-- compilar  ghc -o hw hw.hs
--correr ./hw
-- ctrl + space run in VS
-- ctr + S to save in VS 




let num=9
print(armstrong num)

print(digs 99)