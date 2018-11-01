module PreludeX where
scanr			:: (a -> b -> b) -> b -> [a] -> [b]
scanr f q0 []		=  [q0]
scanr f q0 (x:xs)	=  f x q : qs
			   where qs@(q:_) = scanr f q0 xs 
