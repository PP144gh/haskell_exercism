import Data.Char (toUpper)

-- PANGRAM --
-- checks if the string contains the letter 'letter'. If yes, returns True.
searcher :: String -> Char -> Bool
searcher text letter 
    | text == "" = False
    | head text == letter || head text == toUpper letter  = True 
    | otherwise =  searcher (tail text) letter

-- Does the logic operation 'searcher text letter &  searcher text letter & ...' for all letters in string comp
searcherlist :: String -> [Char] -> Bool
searcherlist _ [] = True
searcherlist text (o:comp) = searcher text o && searcherlist text comp



isPangram :: String -> Bool
isPangram text 
    | length text < 26 = False
    | searcherlist text comp = True 
    | otherwise = False
    where 
        comp = "aeioubcdfghjklmnpqrstvxywz"



-- OR

{-

import Data.Char (toLower)
lowercaseLetters :: String
lowercaseLetters = ['a' .. 'z']
-- all condition list, if all elements of list match condition returns true
isPangram :: String -> Bool
isPangram text = all (`elem` map toLower text) lowercaseLetters

-- map toLower text
--map toLower text , gives a new list with all elements of text acted by the function toLower, so lowercase. lets call this lt.

--all (`elem` map toLower text) lowercaseLetters = all (`elem` lt) lowercaseLetters 
--example of partial application and currying. still dont get it completely.
--for every letter of lowercaseLetters, el, it does el `elem` lt, this is True if the letter el is in lt. all does a concatenation of all these conditions and returns true when all are true.

    -}

---------------
