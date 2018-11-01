module Prelude_List_unzip3 where
unzip3			:: [(a,b,c)] -> ([a],[b],[c])
unzip3			=  foldr (\(a,b,c) ~(as,bs,cs) -> (a:as,b:bs,c:cs))
				 ([],[],[])
