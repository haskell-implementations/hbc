module C_Num(Num(..)) where
class  (Eq a, Text a) => Num a  where
    (+), (-), (*)	:: a -> a -> a
    negate		:: a -> a
    abs, signum		:: a -> a
    fromInteger		:: Integer -> a
    fromInt		:: Int -> a
    {-:"PfromInteger":-}       :: (Integer, Int, Double) -> a

    x - y		=  x + negate y
    fromInt x		=  fromInteger ({-:"PInt2Integer":-} x)
    {-:"PfromInteger":-} x =  case x{-# NOEVAL #-} of { (i, _, _) -> fromInteger i }
