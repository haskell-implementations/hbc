module P_List_partition  where
partition		:: (a -> Bool) -> [a] -> ([a],[a])
partition p		=  foldr select ([],[])
			   where select x (ts,fs) | p x	      = (x:ts,fs)
						  | otherwise = (ts,x:fs)
--SLOW
