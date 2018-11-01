module P_Complex_realPart where
realPart :: (RealFloat a) => Complex a -> a
realPart (x:+y)	 =  x
