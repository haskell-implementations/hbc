module MonadUtil where
(@@)             :: (Monad m) => (a -> m b) -> (c -> m a) -> (c -> m b)
f @@ g           = \ x -> g x >>= f

mapAndUnzipR :: (Monad m) => (a -> m (b,c)) -> [a] -> m ([b], [c])
mapAndUnzipR f xs = accumulateR (map f xs) >>= return . unzip

accumulateR      :: (Monad m) => [m a] -> m [a]
accumulateR      = foldr mcons (return [])
	where mcons p q = q >>= \ xs -> p >>= \ x -> return (x:xs)

zipWithR         :: (Monad m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithR f xs ys = accumulateR (zipWith f xs ys)

sequenceL        :: (Monad m) => [m a] -> m ()
sequenceL []     = return ()
sequenceL (x:xs) = x >> sequenceL xs

sequenceR        :: (Monad m) => [m a] -> m ()
sequenceR []     = return ()
sequenceR (x:xs) = sequenceR xs >> x >> return ()

mapL             :: (Monad m) => (a -> m b) -> ([a] -> m [b])
mapL f []        = return []
mapL f (x:xs)    = f x >>= \ y -> mapL f xs >>= \ ys -> return (y:ys)

mapR             :: (Monad m) => (a -> m b) -> ([a] -> m [b])
mapR f []        = return []
mapR f (x:xs)    = mapR f xs >>= \ ys -> f x >>= \ y -> return (y:ys)

map_             :: (Monad m) => (a -> m b) -> ([a] -> m ())
map_ f []        = return ()
map_ f (x:xs)    = f x >> map_ f xs

foldR            :: (Monad m) => (a -> b -> m b) -> b -> [a] -> m b
foldR f a []     = return a
foldR f a (x:xs) = foldR f a xs >>= \y -> f x y

concatM          :: MonadPlus m => [m a] -> m a
concatM          =  foldr (++) zero

done			:: (Monad m) => m ()
done			= return ()

