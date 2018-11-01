module PreludeX where
accumulate :: (Monad m) => [m a] -> m [a]
accumulate = foldr mcons (return [])
	where mcons p q = p >>= \ x -> q >>= \ xs -> return (x:xs)
