module PreludeX where
span			:: (a -> Bool) -> [a] -> ([a],[a])
span p []		=  ([],[])
span p xs@(x:xs')
	   | p x	=  let (ys,zs) = span p xs' in (x:ys,zs)
	   | otherwise	=  ([],xs)
