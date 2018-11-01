module PreludeX where
scanl			:: (a -> b -> a) -> a -> [b] -> [a]
scanl f q xs		=  q : (case xs of
				[]   -> []
				x:xs -> scanl f (f q x) xs)
