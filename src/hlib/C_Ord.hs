module C_Ord(Ord(..)) where
class  (Eq a) => Ord a  where
    (<), (<=), (>=), (>):: a -> a -> Bool
    max, min		:: a -> a -> a

    x <	 y		=  x <= y && x /= y
    x >= y		=  y <= x
    x >	 y		=  y <	x
    -- The following default methods are appropriate for partial orders.
    -- Note that the second guards in each function can be replaced
    -- by "otherwise" for total orders.
    max x y | x >= y	=  x
	    | y >= x	=  y
	    |otherwise	=  error "PreludeCore.Ord.max: no ordering relation"
    min x y | x <= y	=  x
	    | y <= x	=  y
	    |otherwise	=  error "PreludeCore.Ord.min: no ordering relation"
