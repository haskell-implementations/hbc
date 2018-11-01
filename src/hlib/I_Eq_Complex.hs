module I_Eq_Complex () where
-- Do a manual instance declaration to be able to specialize.
{-# SPECIALIZE instance Eq (Complex Double) #-}
{-# SPECIALIZE instance Eq (Complex Float) #-}
instance (RealFloat a) => Eq (Complex a) where
    (x:+y) == (x':+y')	= x == x' && y == y'
    (x:+y) /= (x':+y')	= x /= x' || y /= y'
