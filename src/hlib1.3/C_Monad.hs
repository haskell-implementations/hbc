module PreludeX where
class  Monad m where
    (>>=)	:: m a -> (a -> m b) -> m b
    (>>)	:: m a -> m b -> m b
    return	:: a -> m a
    x >> y = x >>= \ _ -> y

