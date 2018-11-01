module PreludeX where
mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
mapM_ f as = sequence (map f as)
