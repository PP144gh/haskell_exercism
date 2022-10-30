data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c 
    | a>0 && b>0 && c>0 && maximum [a,b,c] < (sum [a,b,c] - maximum [a,b,c]) = 
        let unique [] = []
            unique (x:xs)
              | x `elem` xs  = unique xs
              | otherwise = x : unique xs in
                if (length . unique $ [a,b,c]) == 2 then Isosceles else if (length . unique $ [a,b,c]) == 3 then Scalene else Equilateral
triangleType _ _ _ = Illegal



main :: IO ()
main = do
{- comentario multiline
-}
--comentario oneline
-- compilar  ghc -o hw hw.hs
--correr ./hw
-- ctrl + space run in VS
-- ctr + S to save in VS 



let dice = [1, 2, 3, 4, 6]
print(triangleType 2 2 2)

