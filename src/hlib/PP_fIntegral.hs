module P_Prelude_fromIntegral(fromIntegral) where
fromIntegral	:: (Integral a, Num b) => a -> b
fromIntegral	=  fromInteger . toInteger
