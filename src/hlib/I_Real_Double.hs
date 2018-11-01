module I_Real_Double where
instance Real Double where
	toRational x	=  (m%1)*(b%1)^^n
			   where (m,n) = decodeFloat x
				 b     = floatRadix  x
