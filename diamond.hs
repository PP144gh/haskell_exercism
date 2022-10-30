
import Data.Char

diamond :: Char -> Maybe [String]
diamond char = if num >= 65 && num<=90 then Just (maker 'A' char ++ (tail . reverse) (maker 'A' char)) else Nothing
    where num = ord char



maker :: Char -> Char -> [String]
maker chari charf
    | numi == 0 = (spacel numf ++ [chari] ++ spacel numf) : maker (chr (numi +66)) charf
    | numi <= numf =  (spacel (numf - numi) ++ [chari] ++ spacel (2*numi -1) ++ [chari] ++ spacel (numf -numi)) : maker (chr (numi +66)) charf
    | otherwise = []
 where
    spacel num = replicate num ' '
    numi = mod (ord chari) 65
    numf = mod (ord charf) 65



printdiamond :: Maybe [String] -> String
printdiamond (Just []) = []
printdiamond list = (head . (\(Just i) -> i) $ list ) ++ "\n" ++ printdiamond (Just (tail . (\(Just i) -> i) $ list))

main :: IO ()
main = do
{- comentario multiline
-}
--comentario oneline
-- compilar  ghc -o hw hw.hs
--correr ./h`
-- ctrl + space run in VS
-- ctr + S to save in VS 





let char = 'G'
print (maker 'A' char)
putStr(printdiamond $ diamond char)