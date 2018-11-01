module P_Complex_cis where
{-# SPECIALIZE cis :: Double -> Complex Double, Float -> Complex Float #-}
cis		 :: (RealFloat a) => a -> Complex a
cis theta	 =  cos theta :+ sin theta
