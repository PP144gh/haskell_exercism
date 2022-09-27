{-# LANGUAGE BlockArguments #-}

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show)


-- data is like a class and inside it are "objects", that why North, East etc are called constructors.


type Pos = (Integer,Integer)


-- class robot with object mkrobot with properties Bearing and Pos

data Robot = MkRobot { dir :: Bearing, coords :: Pos}
    deriving Show

-- this is called record syntax. it better than "MkRobot dir coords" because one can refer to the properties by their name and not by their order in the constructor.
-- AND accessor functions are automatically defined. see bearing and coordinates for examples.

mkRobot :: Bearing -> Pos -> Robot
mkRobot direction coordinates = MkRobot {dir = direction, coords = coordinates}
--or mkRobot direction coordinates = MkRobot direction coordinates


bearing :: Robot -> Bearing
bearing robot = dir robot


rotate :: Robot -> Char -> Bearing
rotate robot char
    | char == 'R' = head (filter (\x -> x /= bearing robot) rlist)
    | char == 'L' = bearing robot
    | otherwise = bearing robot
    where rlist= cycle [North, East, South, West]
          llist = cycle [North, West, South, East]

 



coordinates :: Robot -> Pos
coordinates robot = coords robot



move :: Robot -> String -> Robot
move robot [] = robot
move robot (i:instructions) 
    | i =='R' && currentdir == North = move (MkRobot {dir = East, coords = currentcoords }) instructions
    | otherwise = move robot instructions

    where currentdir = bearing robot
          currentcoords = coordinates robot




main :: IO ()
main = do
{- comentario multiline
-}
--comentario oneline
-- compilar  ghc -o hw hw.hs
--correr ./hw
-- ctrl + space run in VS
-- ctr + S to save in VS 




let dir = East
let pos =(7,3)
let robot=mkRobot dir pos
print(robot)
print(bearing robot)
print(coordinates robot)
print(rotate robot 'R')
print(move robot "R")
