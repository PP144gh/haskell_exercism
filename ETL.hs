
import Data.Map (Map, toList, fromList)
import Data.Char (toLower)


transform :: Map a String -> Map Char a
transform legacyData = fromList . compute $ toList legacyData where
    compute [] = []
    compute listlegacyData = zip (map toLower . snd . head $ listlegacyData) (head $ map (\(number,list) -> replicate (length list) number) listlegacyData) ++ compute (tail listlegacyData)

-- consider list define below 

--(head $ map (\(number,list) -> replicate (length list) creates a list of [1,1,..,] with the lenght of "AEIOU"
-- (map toLower . snd . head $ listlegacyData) transforms "AEIOU" into "aeiou"
-- rest is the use of zip, trivial. head and tail uses are because of the recursive definition.



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

