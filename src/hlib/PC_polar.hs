module P_Complex_polar where
{-# SPECIALIZE polar :: Complex Double -> (Double, Double), Complex Float -> (Float, Float) #-}
polar		 :: (RealFloat a) => Complex a -> (a,a)
polar z		 =  (magnitude z, phase z)
