decode :: String -> String
decode encodedText = error "You need to implement this function."

encode :: String -> String
encode text = error "You need to implement this function."


rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x : xs) = x : rmdups (filter (/= x) xs)

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0 ..], x == x']


test :: Eq a => [a] -> [[Int]]
test word2= [positions x word2 | x <- word2 ]

test2 code = zip (rmdups code) (rmdups $ test code)
--test2 code = [(x,y) | x <- (rmdups code), y <- (rmdups $ test code)]

func ::[(Char, [Int])] -> Int -> (Char, Int)


func [(a,x:xs)] n 
    | n == x = func [(a,xs)] (n+1)
    | n < x = (a, n)
    | otherwise = (a,0)


    
main :: IO ()
main = do
{- comentario multiline
-}
--comentario oneline
-- compilar  ghc -o hw hw.hs
--correr ./h`
-- ctrl + space run in VS
-- ctr + S to save in VS 




let code = "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB"
print(positions 'W' code)
print(rmdups $ test code)
print(test2 code)
print(func (test2 code) 0 )