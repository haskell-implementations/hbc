module PreludeX(Num(..)) where
class  (Eq a, Eval a, Show a) => Num a  where
    (+), (-), (*)	:: a -> a -> a
    negate		:: a -> a
    abs, signum		:: a -> a
    fromInteger		:: Integer -> a
    fromInt		:: Int -> a
    {-:"PfromInteger":-}       :: (Integer, Int, Double) -> a

    x - y		=  x + negate y
    fromInt x		=  fromInteger ({-:"PInt2Integer":-} x)
    {-:"PfromInteger":-} x =  case x{-# NOEVAL #-} of { (i, _, _) -> fromInteger i }
