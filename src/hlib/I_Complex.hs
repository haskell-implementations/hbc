module I_Complex ( Complex((:+)) )  where
infix 6 :+
data  (RealFloat a)     => Complex a = a{-#STRICT#-} :+ a{-#STRICT#-}  deriving (Binary,Text)
--data  (RealFloat a)     => Complex a = a :+ a  deriving (Eq,Binary,Text)
