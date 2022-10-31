


{-

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)


-- data            
newtype Garden = Garden { plant_list :: String}
    deriving Show


garden :: [String] -> String -> Garden
garden students plants = error "You need to implement this function."

lookupPlants :: String -> Garden -> [Plant]
lookupPlants student garden = error "You need to implement this function."

-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

data Plant
      = Violets
      | Clover
      | Radishes
      | Grass
      deriving (Eq, Show)

type Garden = [(String, Char)]
-- each element of garden contains a PlantID (char) and the name of the Student who owns that plant.

toPlant :: Char -> Plant
toPlant 'V' = Violets
toPlant 'C' = Clover
toPlant 'R' = Radishes
toPlant 'G' = Grass
toPlant g= error $ "Unknown char " ++ show g

garden :: [String] -> String -> Garden
garden students plants =  zip dstudents plants
    where doublestring [] = []
          doublestring (x:xs) = x:x:doublestring xs
          students2 = take (div (length plants -1) 4) students
          dstudents = doublestring students2 ++ "null" :doublestring students2

-- even if length of garden is not appropriate for a given list of students, it auto corrects the student list, that's what students2 does. 
-- note that if length of plant_list is 5, only one student chose flowers. if lenght of plant_list is 9, 2 students did. until length is 25 and all 12 did.

-- dstudents transforms that corrected list in a list with a similar structure to plants list. This way a simple zip does the correct matching.
--(student "null" will have '\n')

lookupPlants :: String -> Garden -> [Plant]
lookupPlants name garden_h = map (\(_,b) -> toPlant b) $ filter (\(namel,_) -> namel == name) garden_h
-- from a given gardens filters the plants that belong to student name and after it converts each PlantID (char) to Plant


main :: IO ()
main = do
{- comentario multiline
-}
--comentario oneline
-- compilar  ghc -o hw hw.hs
--correr ./hw
-- ctrl + space run in VS
-- ctr + S to save in VS

let defaultStudents =[ "Alice", "Bob", "Charlie", "David", "Eve", "Fred", "Ginny", "Harriet", "Ileana", "Joseph", "Kincaid", "Larry"]


let plantList = "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV"
--let plantList = "VVCG\nVVRC"


let gardenx = garden defaultStudents plantList

print(gardenx)

print(lookupPlants "Alice" gardenx)
--print(lookupPlants "null" gardenx)
--print(lookupPlants "Bob" gardenx)