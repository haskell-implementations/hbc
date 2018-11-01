module P_List_zip5  where
zip5			:: [a] -> [b] -> [c] -> [d] -> [e] -> [(a,b,c,d,e)]
zip5			=  zipWith5 (\a b c d e -> (a,b,c,d,e))
