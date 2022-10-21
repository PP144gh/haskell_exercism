recite :: Int -> Int -> [String]
recite start stop
    | start <= stop = ["On the " ++ head (sumup start) ++ " day of Christmas my true love gave to me: " ++ (unwords $ tail (sumup start))] ++ recite (start+1) (stop)
    | otherwise = []


unique :: Int -> [String]
unique 0 = [[]]
unique start = case start of 1 -> "first":"a Partridge in a Pear Tree.":[]
                             2 -> "second":"two Turtle Doves, and":[] 
                             3 -> "third":"three French Hens,":[]
                             4 -> "fourth":"four Calling Birds,":[]
                             5 -> "fifth":"five Gold Rings,":[]
                             6 -> "sixth":"six Geese-a-Laying,":[]
                             7 -> "seventh":"seven Swans-a-Swimming,":[]
                             8 -> "eighth":"eight Maids-a-Milking,":[]
                             9 -> "ninth":"nine Ladies Dancing,":[]
                             10 -> "tenth":"ten Lords-a-Leaping,":[]
                             11 -> "eleventh":"eleven Pipers Piping,":[]
                             12 -> "twelfth":"twelve Drummers Drumming,":[]
                             start -> "error":[]
                 
sumup :: Int -> [String]
sumup start
    | start >=1 = list ++ tail  (sumup (start-1))
    | otherwise = [[]]
    where list = unique start


{-
On the first day of Christmas my true love gave to me: a Partridge in a Pear Tree.

On the second day of Christmas my true love gave to me: two Turtle Doves, and a Partridge in a Pear Tree.

On the third day of Christmas my true love gave to me: three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.

On the fourth day of Christmas my true love gave to me: four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.

On the fifth day of Christmas my true love gave to me: five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.

On the sixth day of Christmas my true love gave to me: six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.

On the seventh day of Christmas my true love gave to me: seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.

On the eighth day of Christmas my true love gave to me: eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.

On the ninth day of Christmas my true love gave to me: nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.

On the tenth day of Christmas my true love gave to me: ten Lords-a-Leaping, nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.

On the eleventh day of Christmas my true love gave to me: eleven Pipers Piping, ten Lords-a-Leaping, nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.

On the twelfth day of Christmas my true love gave to me: twelve Drummers Drumming, eleven Pipers Piping, ten Lords-a-Leaping, nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.
-}


main :: IO ()
main = do
{- comentario multiline
-}
--comentario oneline
-- compilar  ghc -o hw hw.hs
--correr ./hw
-- ctrl + space run in VS
-- ctr + S to save in VS 




let n = 3
-- print(tail $ unique 0)
-- print(unique 3)
print(sumup 3)

print(recite 1 12)