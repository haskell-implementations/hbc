module PreludeX where
(&&)		:: Bool -> Bool -> Bool
True  && x	=  x
False && _	=  False
