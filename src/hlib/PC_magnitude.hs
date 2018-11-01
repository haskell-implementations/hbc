module P_Complex_magnitude where
{-# SPECIALIZE magnitude :: Complex Double -> Double, Complex Float -> Float #-}
magnitude :: (RealFloat a) => Complex a -> a
magnitude (x:+y) =  scaleFloat k
		     (sqrt ((scaleFloat mk x)^2 + (scaleFloat mk y)^2))
		    where k  = max (exponent x) (exponent y)
		          mk = - k
--SLOW
