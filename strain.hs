
------ STRAIN ----------------------------

discard :: (a -> Bool) -> [a] -> [a]
-- discard p xs = filter (\x -> not (p x)) xs
discard p xs = [ x | x <- xs, not (p x)] 

keep :: (a -> Bool) -> [a] -> [a]
-- keep p xs = filter (\x -> p x) xs
keep p xs = [ x | x <- xs, p x]

-- for each element x of xs it keeps it if it satisfies condition (p x). (px) synthax can mean x is p (ex: p=even) or xp (ex: p=<10)




---------------------------------------------
