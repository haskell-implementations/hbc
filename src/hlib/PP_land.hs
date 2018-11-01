module P_Prelude_land((&&)) where
(&&)		:: Bool -> Bool -> Bool
True  && x	=  x
False && _	=  False
