module Identity where
--
-- This type is mainly useful as the identity monad.
--

newtype Identity a = I a

instance Functor Identity where
    map f (I a) = I (f a)

instance Monad Identity where
    I x >>= f  =  f x
    return x   =  I x
