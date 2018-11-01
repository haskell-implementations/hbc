module PreludeX where
instance Enum Bool where
	toEnum 0 = False
	toEnum 1 = True
	fromEnum False = 0
	fromEnum True = 1
	enumFrom x		= enumFromTo x True
	enumFromThen x		= enumFromThenTo x True
	enumFromTo False True	= [False,True]
	enumFromTo True  False  = []
	enumFromTo x     _      = [x]
--	enumFromThenTo 
