module I_Ix_Bool where
instance Ix Bool where
	range (False,True) = [False,True]
        range (True,False) = []
        range (x,_)        = [x]
        index (False,_) True  = 1
        index (_,_)     _     = 0
        inRange (False,True) _ = True
        inRange (True,False) _ = False
        inRange (x,_)        y = x == y
