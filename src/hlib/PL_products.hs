module P_List_products  where
products		:: (Num a) => [a] -> [a]
products l		=  scanl (*) 1 l
