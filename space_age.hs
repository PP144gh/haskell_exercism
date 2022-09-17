-- SPACE AGE --
data Planet = Mercury 
            | Venus 
            | Earth 
            | Mars 
            | Jupiter 
            | Saturn 
            | Uranus 
            | Neptune 

earthyeartoseconds :: Float
earthyeartoseconds= 31557600

coef :: Planet -> Float
coef p = case p of
        Mercury -> 0.2408467
        Venus   -> 0.61519726
        Earth   -> 1  
        Mars    -> 1.8808158
        Jupiter -> 11.862615
        Saturn  -> 29.447498
        Uranus  -> 84.016846
        Neptune -> 164.79132

-- equivalent as having the function defined for each case 

ageOn :: Planet -> Float -> Float
ageOn planet seconds   = seconds/(earthyeartoseconds * coef planet)

-- I would prefer a solution where the definition of the data type Planet was merged with the case listing. such that when defining the planets I would link them to their coefficient, like a dict in python. 

-----------------------
