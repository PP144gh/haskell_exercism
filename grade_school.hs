data School = Dummy

add :: Int -> String -> School -> School
add gradeNum student school = error "You need to implement this function."

empty :: School
empty = error "You need to implement this function."

grade :: Int -> School -> [String]
grade gradeNum school = error "You need to implement this function."

sorted :: School -> [(Int, [String])]
sorted school = error "You need to implement this function."



main :: IO ()
main = do
{- comentario multiline
-}
--comentario oneline
-- compilar  ghc -o hw hw.hs
--correr ./hw
-- ctrl + space run in VS
-- ctr + S to save in VS 




let list = [(1,"AEIOU"),(2, "DG")]
--print(compute list)
print(transform $ fromList list)



