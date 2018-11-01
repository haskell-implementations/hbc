module P_Prelude_lor((||)) where
(||)		:: Bool -> Bool -> Bool
True  || _	=  True
False || x	=  x
