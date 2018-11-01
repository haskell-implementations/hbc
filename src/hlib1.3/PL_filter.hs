module PreludeX(filter) where

filterList		:: (a -> Bool) -> [a] -> [a]
filterList p []	= []
filterList p (x:xs)	= if p x then x:filterList p xs else filterList p xs

{-# SPECIALIZE filter :: (a -> Bool) -> [a] -> [a] = filterList #-}
filter :: MonadZero m => (a -> Bool) -> m a -> m a
filter p v = v >>= \x -> if p x then return x else zero
