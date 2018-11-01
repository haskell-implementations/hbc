module Scans where

--foldBoth :: (l -> r -> a -> (r, l) -> l -> r -> [a] -> (r, l)
--mapAccumBoth :: (b -> c -> a -> (b, c, d)) -> b -> c -> [a] -> (b, c, [d])

mapAccumL :: (a -> b -> (a, c)) -> a -> [b] -> (a, [c])
mapAccumL f s []     = (s, [])
mapAccumL f s (x:xs) = (s'', y:ys)
		       where (s',  y)  = f s x
			     (s'', ys) = mapAccumL f s' xs

mapAccumR :: (a -> b -> (a, c)) -> a -> [b] -> (a, [c])
mapAccumR f s []     = (s, [])
mapAccumR f s (x:xs) = (s'', y:ys)
		       where (s'',  y) = f s' x
			     (s',  ys) = mapAccumR f s xs

