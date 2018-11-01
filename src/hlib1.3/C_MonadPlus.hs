module PreludeX where
class (MonadZero m) => MonadPlus m where
    (++) :: m a -> m a -> m a

