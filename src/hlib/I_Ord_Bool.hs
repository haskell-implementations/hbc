module I_Ord_Bool where
instance Ord Bool where
	False <  True  = True
	_     <  _     = False
	True  >  False = True
	_     >  _     = False
	True  <= False = False
	_     <= _     = True
	False >= True  = False
	_     >= _     = True
