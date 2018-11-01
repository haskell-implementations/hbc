module PreludeX(Ord(..)) where
class  (Eq a) => Ord a  where
    (<), (<=), (>=), (>):: a -> a -> Bool
    max, min		:: a -> a -> a
    compare		:: a -> a -> Ordering

    compare x y		= if x == y then EQ else if x <= y then LT else GT

    x <= y		= case compare x y of GT -> False; _ -> True
    x <  y		= case compare x y of LT -> True;  _ -> False
    x >= y		= case compare x y of LT -> False; _ -> True
    x >  y		= case compare x y of GT -> True;  _ -> False

    max x y		= if x >= y then x else y
    min x y		= if x <  y then x else y
