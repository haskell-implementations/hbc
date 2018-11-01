module PreludeX where
applyM            :: (Monad m) => (a -> m b) -> (m a -> m b)
applyM f x        = x >>= f

