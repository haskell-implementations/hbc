module PreludeX where
#include "../lib/flt.h"
import LML_Double

instance  RealFloat Float where
	floatRadix _	= FLT_RADIX
	floatDigits _	= FLT_DIGITS
	floatRange _	= (FLT_MINEXP, FLT_MAXEXP)
	decodeFloat x 	= {-:"Dfdecode":-} x
	encodeFloat x y	= {-:"Dfencode":-} x y
	isNaN x         = {-:"DfisNaN":-} x
	isInfinite x    = {-:"DfisInfinity":-} x
	isDenormalized x= {-:"DfisDenormalized":-} x
	isNegativeZero x= {-:"DfisNegativeZero":-} x
	isIEEE x	= ISIEEE
