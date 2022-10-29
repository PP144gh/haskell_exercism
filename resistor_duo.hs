

data Color =
    Black
  | Brown
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Violet
  | Grey
  | White
  deriving (Eq, Show, Enum, Bounded)

value :: (Color, Color) -> Int
value (a, b) = 10 * fromEnum a + fromEnum b
--using function defined for members of Class Enum. fromEnum returns a Int = order in which the value is defined in the data.

{-
value (a, b) = read $ color_code a ++ color_code b
    where color_code color = case color of
                               Black -> "0"
                               Brown -> "1"
                               Red -> "2"
                               Orange -> "3"
                               Yellow -> "4"
                               Green -> "5"
                               Blue -> "6"
                               Violet -> "7"
                               Grey -> "8"
                               White -> "9"
-}




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
print(value (Brown,Black))

