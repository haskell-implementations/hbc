module P_List_genericLength  where
genericLength		:: (Num a) => [b] -> a
genericLength l		= fromInt (length l)
