module P_List_zip  where
zip			:: [a] -> [b] -> [(a,b)]
zip (x:xs) (y:ys)	=  (x,y) : zip xs ys
zip _      _		= []
