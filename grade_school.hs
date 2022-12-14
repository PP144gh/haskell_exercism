

--recursive types pag 96 book

--data Nat = Zero | Succ Nat  

-- Zero -> Succ Zero

--2nd try with recursive types

import Data.List

-- see School data as a tree with one branch, origin in empty. Each grade is a node that contains a list of names. If grades are ordered, this is a search tree. see page 98 book. 

data School = Empty | Node Int [String] School

empty :: School
empty = Empty

-- Add sorted by grade
add :: Int -> String -> School -> School
add newGrade name Empty = Node newGrade [name] Empty
add newGrade name (Node gr names next)
    | newGrade < gr = Node newGrade [name] (Node gr names next)
    | newGrade == gr = Node gr (insert name names) next
    | otherwise = Node gr names (add newGrade name next)

--lowest grade is the last node. Tree is built like empty -> highest grade -> .... lowest grade. example: Node lowest [] (Node (... (Node highest [] empty)))
-- recursion on the tree starts in the outermost node (the lowest grade). hence newGrade < gr = Node newGrade [name] (Node gr names next), if newgrade is smaller than the outermost grade node, it will create another node in the end of the tree.
--if newgrade already exists, it only adds to the list of names using insert function.
-- if newgrade doesnt exist and is larger than current node grade, keep going in the innermost direction until current node grade is larger than new grade or you find empty and add node there.   

-- The structure is already sorted, just print it
sorted :: School -> [(Int, [String])]
sorted Empty = []
sorted (Node gr names next) = (gr, names) : sorted next

--Tree is built like specified above such that the sorting is trivial, it will just print starting from the outermost/last node, which is the one with the lowest grade.

grade :: Int -> School -> [String]
grade _ Empty = []
grade g (Node gr names next)
    | g == gr = names
    | otherwise = grade g next








--1st try

{-
import Data.List


data School = Students { names :: [String], grades :: [Int]} 
    deriving Show

add :: Int -> String -> School -> School
add gradeNum student school = Students { names = student : names school , grades = gradeNum : grades school}

empty :: School
empty = Students [] []

grade :: Int -> School -> [String]
grade gradeNum school = sort $ map fst $ filter (\(_,gradex) -> gradex == gradeNum ) (zip (names school) (grades school))

sorted :: School -> [(Int, [String])]
--sorted Empty = []
sorted school = func un_grades
    where zipped = zip (grades school) (names school)
          un_grades = rmdups (grades school)
          func [] = []
          func g = (head g, sort $ map snd (filter (\(gradex,_) -> gradex == head g ) zipped)) : func (tail g)


rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

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


--1st it
--let nameslist= ["Alice", "John", "Bob"]
--let gradeslist = [2,3,3]
--let school = Students nameslist gradeslist -- 1st it
let school = add 3 "John" (add 3 "Bob" (add 2 "Alice" empty))


--print(grades school)
print(grade 3 school)
let school2 = add 3 "Alex" school

print(sorted school)
print(sorted school2)



