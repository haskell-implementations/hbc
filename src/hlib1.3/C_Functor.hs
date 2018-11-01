module PreludeX where
class  Functor f where
    map :: (a -> b) -> (f a -> f b)

