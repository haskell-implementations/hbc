module I_Fractional_Complex ( )  where
{-# SPECIALIZE instance Fractional (Complex Double) #-}
{-# SPECIALIZE instance Fractional (Complex Float) #-}
instance  (RealFloat a) => Fractional (Complex a)  where
    (x:+y) / (x':+y')	=  (x*x''+y*y'') / d :+ (y*x''-x*y'') / d
			   where x'' = scaleFloat k x'
				 y'' = scaleFloat k y'
				 k   = - max (exponent x') (exponent y')
				 d   = x'*x'' + y'*y''

    fromRational a	=  fromRational a :+ 0
