---- Guessing Game ------------------


reply :: Int -> String
reply guess
    | guess == 42 = "Correct"
    | guess == 41 || guess == 43 = "So close"
    | guess >43 = "Too high"
    | guess<41 = "Too low"
    | otherwise = "Non-Valid number. Choose between 1 and 100."


-----------------------------------------------------------------
