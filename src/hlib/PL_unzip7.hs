module Prelude_List_unzip7 where
unzip7			:: [(a,b,c,d,e,f,g)] -> ([a],[b],[c],[d],[e],[f],[g])
unzip7			=  foldr (\(a,b,c,d,e,f,g) ~(as,bs,cs,ds,es,fs,gs) ->
					(a:as,b:bs,c:cs,d:ds,e:es,f:fs,g:gs))
				 ([],[],[],[],[],[],[])

