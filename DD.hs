import Test.QuickCheck

data Character = Character
  { strength     :: Int
  , dexterity    :: Int
  , constitution :: Int
  , intelligence :: Int
  , wisdom       :: Int
  , charisma     :: Int
  , hitpoints    :: Int
  }
  deriving (Show, Eq)

modifier :: Int -> Int
modifier constitution = (constitution-10) `div` 2
-- or
-- modifier = (`div` 2) . subtract 10    this is a composition of functions (. is the composition operator). first act on the right. so, subtract 10 and them do integer division by 2

ability :: Gen Int
ability = do
    d1 <- dice
    d2 <- dice
    d3 <- dice
    d4 <- dice
    let list =[d1,d2,d3,d4]
    -- return (sum (filter (\x -> x /= minimum list) list))   
    --doesnt work because when there are two or more equal minima he removes all of them.
    return (sum list - minimum list)

character :: Gen Character
character = do
    str <- ability
    dex <- ability
    con <- ability
    int <- ability
    wis <- ability
    char <- ability
    let hp=10+ modifier con
    return Character { strength = str, dexterity = dex, constitution = con, intelligence = int, wisdom = wis, charisma = char, hitpoints = hp }
  

-- do notation used to perform a sequence of IO actions. action dice and store its result in variable d1: d1 <- dice
--still not sure of what the bind operator (<-) means. In ability there is no type transformation from dice to d1, but in the main func there is a type transformation? This is connected to monads?
-- COME BACK IN THE FUTURE. SEE PAGE 166 OF THE BOOK. this is a monad and the thing in main is a example of "interactive programming?" (see chap 10)

dice :: Gen Int
dice = choose (1, 6)



main :: IO ()
main = do
{- comentario multiline
-}
--comentario oneline
-- compilar  ghc -o hw hw.hs
--correr ./hw
-- ctrl + space run in VS
-- ctr + S to save in VS


--QuickCheck logic
-- things have type Gen a, such that generate foo, where foo has type Gen a, gives IO a. Then we need to bind (ex: n <- ioN) to transform from IO a to a. See examples below



let list = [1,2,3,4]
let ioN= generate dice
n <- ioN

let iochar = generate character
char <- iochar

let ioabi= generate ability
abi <- ioabi

--An IO a (a can be Int, Float, etc) is not an a, it's a promise to read an Integer later. There is no instance of `Show` for `IO Integer` as the compiler says. Bind operator (<-) is needed to convert from IO Int to Int.
-- This is related to Monads, so in the future it will make more sense.

print(abi)
print(n)
print(char)