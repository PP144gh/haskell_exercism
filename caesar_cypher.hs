import Data.Char

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs ] 


shift :: Int -> Char -> Char
shift n x
    | isLower x = inttoc ((ctoint x 'a' +n) `mod` 26) 'a'
    | isUpper x = inttoc ((ctoint x 'A' +n) `mod` 26) 'A'
    | otherwise = x

caeser :: String -> String
caeser xs = encode 3 xs

decode_caeser :: String -> String
decode_caeser xs = encode (-3) xs


ctoint :: Char -> Char -> Int
ctoint c s = ord c - ord s

inttoc :: Int -> Char -> Char
inttoc n s= chr (ord s + n)

--ascii lower case letters [97,122]
--asci upper case letters [65,90]

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0 ..], x == x']

table :: [Float]
table =
  [ 8.1
  , 1.5
  , 2.8
  , 4.2
  , 12.7
  , 2.2
  , 2.0
  , 6.1
  , 7.0
  , 0.2
  , 0.8
  , 4.0
  , 2.4
  , 6.7
  , 7.5
  , 1.9
  , 0.1
  , 6.0
  , 6.3
  , 9.0
  , 2.8
  , 1.0
  , 2.4
  , 0.2
  , 2.0
  , 0.1
  ]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

alphabets :: [Char] -> Int
alphabets xs =
  length [x | x <- xs, (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z')]

freqs :: String -> [Float]
freqs xs =
  [ percent ((count l xs) + (count u xs)) n
  | (l, u) <- zip ['a' .. 'z'] ['A' .. 'Z']
  ]
  where
    n = alphabets xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e) ^ 2) / e | (o, e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: [Char] -> [Char]
crack xs = encode (-factor) xs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n <- [0 .. 25]]
    table' = freqs xs


-- minimization of the chi square of the distribution of letters of the sentence compared with the distribution obtained from a big set of data (see table)

main :: IO ()
main = do
{- comentario multiline
-}
--comentario oneline
-- compilar  ghc -o hw hw.hs
--correr ./hw
-- ctrl + space run in VS
-- ctr + S to save in
let str="Haskell is fun and beautiful."
print(caeser str)
print(decode_caeser (caeser str))
print(crack (caeser str))