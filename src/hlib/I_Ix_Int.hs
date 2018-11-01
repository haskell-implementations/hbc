module I_Ix_Int() where
instance Ix Int where
	range (l,h) = irange l h
	index (l,h) x = x - l
	inRange (l,h) x = l <= x && x <= h

irange :: Int->Int->[Int]
irange l h = if l > h then [] else l : irange (l+1) h
