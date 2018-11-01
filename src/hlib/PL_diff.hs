module P_List_diff  where
infix  5  \\
(\\)			:: (Eq a) => [a] -> [a] -> [a]
(\\)			=  foldl del
			   where [] `del` _	    = []
				 (x:xs) `del` y
					| x == y    = xs
					| otherwise = x : xs `del` y
--SLOW
