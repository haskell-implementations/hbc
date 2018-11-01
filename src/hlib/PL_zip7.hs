module P_List_zip7  where
zip7			:: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [(a,b,c,d,e,f,g)]
zip7			=  zipWith7 (\a b c d e f g -> (a,b,c,d,e,f,g))
