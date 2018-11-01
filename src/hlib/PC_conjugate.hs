module P_Complex_conjugate where
{-# SPECIALIZE conjugate :: Complex Double -> Complex Double, Complex Float -> Complex Float #-}
conjugate	 :: (RealFloat a) => Complex a -> Complex a
conjugate (x:+y) =  x :+ (-y)
