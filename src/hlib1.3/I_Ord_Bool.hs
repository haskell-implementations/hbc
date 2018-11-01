module PreludeX where
instance Ord Bool where
        compare x y = if x < y then LT else if x > y then GT else EQ
	False <  True  = True
	_     <  _     = False
	True  >  False = True
	_     >  _     = False
	True  <= False = False
	_     <= _     = True
	False >= True  = False
	_     >= _     = True
