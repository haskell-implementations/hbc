module P_Complex_mkPolar where
{-# SPECIALIZE mkPolar :: Double -> Double -> Complex Double, Float -> Float -> Complex Float #-}
mkPolar		 :: (RealFloat a) => a -> a -> Complex a
mkPolar r theta	 =  r * cos theta :+ r * sin theta
