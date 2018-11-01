module P_List_break  where
break		:: (a -> Bool) -> [a] -> ([a],[a])
break p []		=  ([],[])
break p xs@(x:xs')
	   | p x	=  ([],xs)
	   | otherwise	=  let (ys,zs) = break p xs' in (x:ys,zs)
