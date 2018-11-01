module Monad where

join			:: (Monad m) => m (m a) -> m a
join x			=  x >>= id

mapAndUnzipM		:: (Monad m) => (a -> m (b,c)) -> [a] -> m ([b], [c])
mapAndUnzipM f xs	=  accumulate (map f xs) >>= return . unzip

zipWithM		:: (Monad m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM f xs ys	=  accumulate (zipWith f xs ys)

foldM			:: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM f a []		=  return a
foldM f a (x:xs)	=  f a x >>= \ y -> foldM f y xs

when			:: (Monad m) => Bool -> m () -> m ()
when p s		=  if p then s else return ()

unless			:: (Monad m) => Bool -> m () -> m ()
unless p s		=  if p then return () else s

ap			:: (Monad m) => m (a -> b) -> m a -> m b
ap f x			= do f' <- f
			     x' <- x
                             return (f' x')

liftM			:: (Monad m) => (a -> b) -> (m a -> m b)
liftM f x		= do x' <- x
			     return (f x')

liftM2			:: (Monad m) => (a -> b -> c) -> (m a -> m b -> m c)
liftM2 f x y		= do x' <- x
			     y' <- y
			     return (f x' y')

liftM3			:: (Monad m) => (a -> b -> c -> d) -> (m a -> m b -> m c -> m d)
liftM3 f x y z		= do x' <- x
			     y' <- y
			     z' <- z
			     return (f x' y' z')

liftM4			:: (Monad m) => (a -> b -> c -> d -> e) -> (m a -> m b -> m c -> m d -> m e)
liftM4 f x y z a	= do x' <- x
			     y' <- y
			     z' <- z
			     a' <- a
			     return (f x' y' z' a')

liftM5			:: (Monad m) => (a -> b -> c -> d -> e -> f) -> (m a -> m b -> m c -> m d -> m e -> m f)
liftM5 f x y z a b	= do x' <- x
			     y' <- y
			     z' <- z
			     a' <- a
			     b' <- b
			     return (f x' y' z' a' b')

