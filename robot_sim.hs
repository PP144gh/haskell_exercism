{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use bimap" #-}

import Data.List ( elemIndex ) 
import Data.Maybe ( fromMaybe )

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

coordinates :: Robot -> Pos
coordinates robot = coords robot

rotate :: Robot -> Char -> Bearing
rotate robot char
    | char == 'R' = rlist !! (fromMaybe (-2) (elemIndex (bearing robot) rlist) +1) -- gives -2 if bearing robot is not in the list (which should be impossible)
    | char == 'L' = llist !! (fromMaybe (-2) (elemIndex (bearing robot) llist) +1)
    | otherwise = bearing robot
    where rlist= cycle [North, East, South, West]
          llist = cycle [North, West, South, East]


go :: Robot -> Char -> Pos
go robot char 
    | char =='A' = if bearing robot == North then (fst (coordinates robot), snd (coordinates robot) + 1) else
        if bearing robot == East then (fst (coordinates robot) + 1, snd (coordinates robot)) else
            if bearing robot == South then (fst (coordinates robot), snd (coordinates robot) - 1) else
                if bearing robot == West then (fst (coordinates robot) - 1, snd (coordinates robot)) else coordinates robot
   -- | char == 'L' || char == 'R' = go (MkRobot {dir = rotate robot char, coords = coordinates robot }) 'A'   this line was for the case R meant rotating right and moving forward, for instance.
    | otherwise = coordinates robot




move :: Robot -> String -> Robot
move robot [] = robot
--move robot (i:instructions) = move (MkRobot {dir = rotate robot i, coords = go robot i }) instructions
move robot instructions = foldl (\ robot i -> MkRobot {dir = rotate robot i, coords = go robot i}) robot instructions -- same as above but using foldl


main :: IO ()
main = do
{- comentario multiline
-}
--comentario oneline
-- compilar  ghc -o hw hw.hs
--correr ./hw
-- ctrl + space run in VS
-- ctr + S to save in VS 




let dir = North
let pos =(7,3)
let robot=mkRobot dir pos
let instructions = "RAALAL"
let rlist= cycle [North, East, South, West]


--print(rlist)

print(robot)
print(bearing robot)
print(coordinates robot)
print(move robot instructions)


