module P_List_zip4  where
zip4			:: [a] -> [b] -> [c] -> [d] -> [(a,b,c,d)]
zip4			=  zipWith4 (\a b c d -> (a,b,c,d))
