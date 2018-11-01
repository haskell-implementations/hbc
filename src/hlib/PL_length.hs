module P_List_length(length)  where
length			:: [a] -> Int
length l		=  len l 0
len :: [a]->Int->Int
len []     a = a
len (_:xs) a = len xs (a+1)
