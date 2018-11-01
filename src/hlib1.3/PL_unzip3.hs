module PreludeX where
unzip3			:: [(a,b,c)] -> ([a],[b],[c])
unzip3			=  foldr (\(a,b,c) ~(as,bs,cs) -> (a:as,b:bs,c:cs))
				 ([],[],[])
