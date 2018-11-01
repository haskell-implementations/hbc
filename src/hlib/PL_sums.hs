module P_List_sums  where
sums		:: (Num a) => [a] -> [a]
sums l		=  scanl (+) 0 l
