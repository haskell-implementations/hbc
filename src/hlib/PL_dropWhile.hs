module P_List_dropWhile  where
dropWhile		:: (a -> Bool) -> [a] -> [a]
dropWhile p []		=  []
dropWhile p xs@(x:xs')
	    | p x       =  dropWhile p xs'
	    | otherwise =  xs
