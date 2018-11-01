module PreludeX where
sequence :: (Monad m) => [m a] -> m ()
sequence = foldr (>>) (return ())
