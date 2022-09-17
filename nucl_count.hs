-- Nucleotide Count --

import Data.Map (Map(), fromList)


data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)


nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = if filter (\x -> x/='G' && x/='C' && x/='T' && x/='A') xs == ""
           then Right (fromList  [(A,length (filter (== 'A') xs)), (G,length (filter (== 'G') xs)), (C,length (filter (== 'C') xs)), (T,length (filter (== 'T') xs)) ])
           else Left "Invalid"






------------------------------------------
