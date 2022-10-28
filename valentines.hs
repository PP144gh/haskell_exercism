-- Define the function and required algebraic data types (ADT) below.

data Approval = Yes | No | Maybe

data Cuisine = Korean | Turkish

data Genre = Crime | Horror | Romance | Thriller

data Activity = BoardGame | Chill | Movie Genre | Restaurant Cuisine | Walk Int


{- 1st it
rateActivity :: Activity -> Approval
rateActivity activity = 
    case activity of
        BoardGame -> No
        Chill -> No
        Movie Romance -> Yes
        Movie other -> No
        Restaurant Korean -> Yes
        Restaurant Turkish -> Maybe
        Walk km -> func km where 
            func k
                | k < 3 = Yes
                | k >=3 && k<=5 = Maybe
                | otherwise = No


-}
--simplification
rateActivity :: Activity -> Approval
rateActivity activity = 
    case activity of
        Movie Romance -> Yes
        Restaurant Korean -> Yes
        Restaurant Turkish -> Maybe
        Walk km | km < 3 -> Yes
        Walk km | km >= 3 && km <= 5 -> Maybe
        _ -> No


main :: IO ()
main = do
{- comentario multiline
-}
--comentario oneline
-- compilar  ghc -o hw hw.hs
--correr ./hw
-- ctrl + space run in VS
-- ctr + S to save in VS 




let num=9
print(num)


