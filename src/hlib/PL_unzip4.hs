module Prelude_List_unzip4 where
unzip4			:: [(a,b,c,d)] -> ([a],[b],[c],[d])
unzip4			=  foldr (\(a,b,c,d) ~(as,bs,cs,ds) ->
					(a:as,b:bs,c:cs,d:ds))
				 ([],[],[],[])
