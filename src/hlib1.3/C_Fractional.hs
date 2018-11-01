module PreludeX(Fractional(..)) where
class  (Num a) => Fractional a  where
    (/)			:: a -> a -> a
    recip		:: a -> a
    fromRational	:: Rational -> a
    {-:"PfromRational":-}      :: (Rational, Double) -> a

    recip x		=  1 / x
    {-:"PfromRational":-} x =  case x{-# NOEVAL #-} of { (r, _) -> fromRational r }

