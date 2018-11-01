module I_Eq_Bool where
instance Eq Bool where
	False == False = True
        True  == True  = True
        _     == _     = False
	False /= False = False
        True  /= True  = False
        _     /= _     = True
