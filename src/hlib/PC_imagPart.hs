module P_Complex_imagPart where
imagPart :: (RealFloat a) => Complex a -> a
imagPart (x:+y)	 =  y
