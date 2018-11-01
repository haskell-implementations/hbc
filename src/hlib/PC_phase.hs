module P_Complex_phase where
{-# SPECIALIZE phase :: Complex Double -> Double, Complex Float -> Float #-}
phase :: (RealFloat a) => Complex a -> a
phase (x:+y)	 =  atan2 y x
