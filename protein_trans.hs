---- PROTEIN TRANSLATION -----

import Data.List (unfoldr)
import Data.Maybe ( fromMaybe )


sequencer :: String -> Maybe String
sequencer rna = case rna of "AUG" -> Just "Methionine"
                            "UUU" -> Just "Phenylalanine"
                            "UUC" -> Just "Phenylalanine"
                            "UUA" -> Just "Leucine"
                            "UUG" -> Just "Leucine"
                            "UCU" -> Just "Serine"
                            "UCC" -> Just "Serine"
                            "UCA" -> Just "Serine"
                            "UCG" -> Just "Serine"
                            "UAU" -> Just "Tyrosine"
                            "UAC" -> Just "Tyrosine"
                            "UGU" -> Just "Cysteine"
                            "UGC" -> Just "Cysteine"
                            "UGG" -> Just "Tryptophan"
                           -- "UAA" -> Nothing 
                          --  "UAG" -> Nothing
                           -- "UGA" -> Nothing
                            rna -> Nothing



helper :: String -> Maybe (String,String)
helper rna 
     | sequencer (take 3 rna) == Nothing = Nothing
     | otherwise = Just (fromMaybe [] (sequencer (take 3 rna)), drop 3 rna)

-- could have used splitAt 3

proteins :: String -> Maybe [String]
proteins ""  = Nothing
proteins rna = Just (unfoldr helper rna)



-- foldr  takes a function, a value and a list. it operates (using the function) the value and the first element of the list. does this iteratively until the list ends.
--unfoldr takes a function f that receives a seed value. this function f naturally divides the seed into chunks and operates iteratively. unfoldr returns a list with the results of the application of f
-- to the chunks of the seed value. function f should have a Nothing instruction such that unfoldr knows when to stop.



----------------------------------------
