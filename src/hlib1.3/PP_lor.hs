module PreludeX where
(||)		:: Bool -> Bool -> Bool
True  || _	=  True
False || x	=  x
