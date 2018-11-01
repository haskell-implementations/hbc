module P_List_zip6  where
zip6			:: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [(a,b,c,d,e,f)]
zip6			=  zipWith6 (\a b c d e f -> (a,b,c,d,e,f))
