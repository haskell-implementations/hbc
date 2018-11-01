module I_Enum_Bool where
instance Enum Bool where
	enumFrom x		= enumFromTo x True
	enumFromThen x		= enumFromThenTo x True
	enumFromTo False True	= [False,True]
	enumFromTo True  False  = []
	enumFromTo x     _      = [x]
--	enumFromThenTo 