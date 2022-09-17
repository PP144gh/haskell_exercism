---- ACRONYM -----------


import Data.Char 
import Data.List
import Data.Maybe

findString :: (Eq a) => [a] -> [a] -> Maybe Int
findString search str = findIndex (isPrefixOf search) (tails str)


abbreviate :: String -> String
abbreviate [] = []
abbreviate xs = filter (\x -> x/='-') (map toUpper (take 1 list4) ++ abbreviate list3)
                where list2 = words (filter (\x -> x/='_') xs)
                      list5 = map isUpper (tail (head list2))
                      nCap = findIndex (==True) list5
                      ntra = findString ("-") (head (take 1 list2))
                      tuple
                          | isJust ntra =  (fst (splitAt (fromJust ntra +1) (head list2)), snd (splitAt (fromJust ntra +1) (head list2)) ++ " " ++ unwords (tail list2))
                          | nCap /= Just 0 && nCap /= Nothing = (fst (splitAt (fromJust nCap +1) (head list2)), snd (splitAt (fromJust nCap +1) (head list2)) ++  unwords (tail list2))
                          | otherwise = (head list2,unwords (tail list2))

                      list4 = fst tuple
                      list3 = snd tuple


--ntra is needed to split hifenized words into two
-- nCap same thing for words like HyperText. nCap /= Just 0 is to take into account cases like GNU where we only want to save the G.



---------------------------------------------
