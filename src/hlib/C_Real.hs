module C_Real(Real(..)) where
class  (Num a, Enum a) => Real a  where
	toRational	:: a -> Rational

