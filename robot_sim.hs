
data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show)


-- data is like a class and inside it are "objects", that why North, East etc are called constructors.


type Pos = (Integer,Integer)


-- class robot with object mkrobot with properties Bearing and Pos

data Robot = MkRobot { direction :: Bearing, coords :: Pos}
    deriving Show

-- this is called record syntax. it better than MkRobot direction coordinates because one can refer to the properties by their name and not by their order in the constructor.
-- AND accessor functions are automatically defined. see bearing and coordinates.

mkRobot :: Bearing -> Pos -> Robot
mkRobot direction coordinates = MkRobot direction coordinates


bearing :: Robot -> Bearing
bearing robot = direction robot



coordinates :: Robot -> Pos
coordinates robot = coords robot



move :: Robot -> String -> Robot
move robot instructions = error "You need to implement this function."




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
let pos =(0,0)
let robot=mkRobot dir pos
print(robot)
print(bearing robot)
print(coordinates robot)
