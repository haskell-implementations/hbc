module PreludeX where
#include "../lib/flt.h"
import LML_Double

instance  RealFloat Double where
	floatRadix _	= DBL_RADIX
	floatDigits _	= DBL_DIGITS
	floatRange _	= (DBL_MINEXP, DBL_MAXEXP)
	decodeFloat x 	= {-:"Ddecode":-} x
	encodeFloat x y	= {-:"Dencode":-} x y
	isNaN x         = {-:"DisNaN":-} x
	isInfinite x    = {-:"DisInfinity":-} x
	isDenormalized x= {-:"DisDenormalized":-} x
	isNegativeZero x= {-:"DisNegativeZero":-} x
	isIEEE x	= ISIEEE
