module PreludeX where
concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f []	   = []
concatMap f (x:xs) =
	case f x of
	[] -> concatMap f xs
	ys -> ys ++ concatMap f xs
