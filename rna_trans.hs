-- RNA TRANSCRIPTION --

convertNuc ::  Char -> Char
convertNuc t = case t of 'G' -> 'C'
                         'C' -> 'G'
                         'T' -> 'A'
                         'A' -> 'U'
                         t -> t
 

--list :: [Char]
--list = ['G','C','T','A']
--isDNA :: String -> Bool
--isDNA text = all (`elem` list) text


toRNA :: String -> Either Char String
toRNA "" = Right ""
toRNA xs = if filter (\x -> x/='G' && x/='C' && x/='T' && x/='A') xs == ""
           then Right (map convertNuc xs)
           else Left inval
           where inval = head (filter (\x -> x/='G' && x/='C' && x/='T' && x/='A') xs)
           --the conditions in filter are an example of a lambda function    

toRNA :: String -> String
toRNA "" = ""
toRNA ('G':xs) = 'C':toRNA xs
toRNA ('C':xs) = 'G':toRNA xs
toRNA ('T':xs) = 'A':toRNA xs
toRNA ('A':xs) = 'U':toRNA xs
toRNA (x:xs) = x:toRNA xs    


toRNA :: String -> Either Char String
toRNA = traverse fromDNA
  where
    fromDNA :: Char -> Either Char Char
    fromDNA 'G' = pure 'C'
    fromDNA 'C' = pure 'G'
    fromDNA 'T' = pure 'A'
    fromDNA 'A' = pure 'U'
    fromDNA c = Left c
------------------------

