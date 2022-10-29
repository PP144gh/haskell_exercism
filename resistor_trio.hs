



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
  deriving (Show, Enum, Bounded)

newtype Resistor = Resistor { bands :: (Color, Color, Color) }
  deriving Show

label :: Resistor -> String
label resistor
    | kilo > 1 && mega < 1 = show (round kilo) ++ " kiloohms"
    | mega > 1 && giga < 1 = show (round mega) ++ " megaohms"
    | giga > 1  = show (round giga) ++ " gigaohms"
    | otherwise = show (round ohm) ++ " ohms"
    where ohm = fromIntegral $ ohms resistor
          kilo =(/) ohm 1e3 
          mega = (/) ohm 1e6
          giga = (/) ohm 1e9


ohms :: Resistor -> Int
ohms resistor = (\(a,b,c) -> (10 * fromEnum a + fromEnum b) * 10^fromEnum c) $ bands resistor













main :: IO ()
main = do
{- comentario multiline
-}
--comentario oneline
-- compilar  ghc -o hw hw.hs
--correr ./hw
-- ctrl + space run in VS
-- ctr + S to save in VS 




let resistor = Resistor(Red, Black, Red)
print(bands resistor)
print(ohms resistor)
print((/) (fromIntegral $ ohms resistor) 1e3)
print(label resistor)