module I_Integer() where
instance Ix Integer where
	range (l,h) = irange l h
	index (l,h) x = fromInteger (x - l)
	inRange (l,h) x = l <= x && x <= h

irange :: Integer->Integer->[Integer]
irange l h = if l > h then [] else l : irange (l+1) h
