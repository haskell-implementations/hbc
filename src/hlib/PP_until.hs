module P_Prelude_until(until) where
until			:: (a -> Bool) -> (a -> a) -> a -> a
until p f x | p x	=  x
	    | otherwise =  until p f (f x)
