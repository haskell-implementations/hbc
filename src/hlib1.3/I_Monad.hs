module PreludeX where
import LMLappend

instance Functor [] where
    map f [] = []
    map f (x:xs) = f x : map f xs

instance Monad [] where
    xs >>= f = concatMap f xs
    return x = [x]

instance MonadZero [] where
    zero = []

instance MonadPlus [] where
    xs ++ ys = {-:"_@":-} xs ys

instance Functor Maybe where
    map f Nothing = Nothing
    map f (Just x) = Just (f x)

instance Monad Maybe where
    Nothing >>= _ = Nothing
    Just x  >>= f = f x
    return x      = Just x

instance MonadZero Maybe where
    zero = Nothing

instance MonadPlus Maybe where
    Nothing ++ ys = ys
    xs      ++ ys = xs

