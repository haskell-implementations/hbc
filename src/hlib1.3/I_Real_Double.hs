module PreludeX where
import Ratio
instance Real Double where
	toRational x	=  (m%1)*(b%1)^^n
			   where (m,n) = decodeFloat x
				 b     = floatRadix  x
