-- BOB --

import Data.Char

--helper function (pattern matching)
sentenceType :: String -> String
sentenceType sentence = if last sentence /= ' '
                        then classify (last sentence)
                        else sentenceType (init sentence)
                        where classify '.' = "statement"
                              classify '?' = "question"
                              classify '!' = "exclamation"
                              classify _   = "not a sentence"



allUpper :: String -> Bool
allcharbutlowercase :: String
allcharbutlowercase = map chr [0 .. 95] ++ map chr [123 .. 127]
--ascii codes for all characters but lower cases 96-122
allcharbutuppercase :: String
allcharbutuppercase = map chr [0 .. 64] ++ map chr [91 .. 127]
--ascii codes for all characters but lower cases 65-90
allUpper "" = False
allUpper text = all (`elem` allcharbutlowercase ) (init text) && not (all (`elem` allcharbutuppercase ) (init text))
--init to remove the pontuation mark.
--concatenation of not having lower case and not not having uppercase


allBlank:: String -> Bool
list :: String
list = [' ' .. ' '] ++ ['\t' .. '\t'] ++ ['\n' .. '\n'] ++ ['\r' .. '\r']
-- \t is tabs, \n is newline, \r is return
allBlank text = all (`elem` list) text


responseFor :: String -> String
responseFor xs 
    | xs == "How are you?" = "Sure"
    | allBlank xs = "Fine. Be that way!"
    | allUpper xs && sentenceType xs /= "question"=  "Whoa, chill out!"
    | sentenceType xs == "question" && not (allUpper xs)= "Sure."
    | sentenceType xs == "question" && allUpper xs ="Calm down, I know what I'm doing!"
    | otherwise = "Whatever."


    import           Data.Char
responseFor :: String -> String
responseFor input
    | null text = "Fine. Be that way!"
    | isShouting && isAsking = "Calm down, I know what I'm doing!"
    | isShouting = "Whoa, chill out!"
    | isAsking = "Sure."
    | otherwise = "Whatever."
  where
    text = filter (not . isSpace) input
    letters = filter isLetter text
    isShouting = all isUpper letters && any isUpper letters
    isAsking = last text == '?'

-------------------
