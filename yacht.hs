{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

data Category = Ones
              | Twos
              | Threes
              | Fours
              | Fives
              | Sixes
              | FullHouse
              | FourOfAKind
              | LittleStraight
              | BigStraight
              | Choice
              | Yacht



fHouse :: (Eq a, Num a) => [a] -> a
fHouse dice
    | (length . eqfil $ dice) == 2 = if  (length . eqfil . neqfil $ dice) == 3 then sum dice else 0
    | (length . eqfil $ dice) == 3 = if  (length . eqfil . neqfil $ dice) == 2 then sum dice else 0
    | otherwise = 0
    where eqfil d = filter (== head d) d
          neqfil d = filter (/= head d) d


fourKind :: (Eq a, Num a) => [a] -> a
fourKind dice
  | (length . filter (==head dice) $ dice) == 4 || (length . filter (==head dice) $ dice) == 5 = 4 * (head $ dice)
  | (length . filter (==head dice) $ dice) == 1 = fourKind $ tail dice
  | otherwise = 0


str :: [Int] -> Int -> Int -> Int
str dice num1 num2 = if length (unique dice) == 5 && elem num1 dice && notElem num2 dice then 30 else 0
                where unique [] = []
                      unique (x:xs)
                        | x `elem` xs  = unique xs
                        | otherwise = x : unique xs



yacht :: Category -> [Int] -> Int
yacht Ones dice = length (filter (==1) dice)
yacht Twos dice = 2*length (filter (==2) dice)
yacht Threes dice = 3*length (filter (==3) dice)
yacht Fours dice = 4*length (filter (==4) dice)
yacht Fives dice = 5*length (filter (==5) dice)
yacht Sixes dice= 6*length (filter (==6) dice)
yacht FullHouse dice = fHouse dice
yacht FourOfAKind dice = fourKind dice
yacht LittleStraight dice = str dice 1 6
yacht BigStraight dice = str dice 6 1
yacht Choice dice = sum dice
yacht Yacht dice = div (length . filter (== head dice) $ dice) 5 * 50



-- == is not defined for Category so we use pattern matching



main :: IO ()
main = do
{- comentario multiline
-}
--comentario oneline
-- compilar  ghc -o hw hw.hs
--correr ./hw
-- ctrl + space run in VS
-- ctr + S to save in VS 




let dice = [1, 2, 3, 4, 6]
let category = LittleStraight
print(length . filter (==head dice) $ dice)
print( (length . filter (==head dice) $ dice) == 5)
print(yacht category dice)