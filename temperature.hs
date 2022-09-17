-- TEMPERATURE --
{- Implement the function `tempToC` to convert
`  Fahrenheit to Celsius    -}

coefficient :: Float
coefficient = 1.8

tempfix::Integer
tempfix = 32

tempToC :: Integer -> Float
tempToC temp = fromIntegral ((temp - tempfix))/coefficient

-- toFloat function doesnt work?

{- Implement the function `tempToF` to convert
`  Celsius to Fahrenheit                    -}

tempToF :: Float -> Integer
tempToF temp = ceiling (coefficient*temp + fromIntegral tempfix)

--tempToF temp = ceiling $ 1.8*temp + 32
-- round function rounds down
-- ceiling functions rounds up


-----------------------

