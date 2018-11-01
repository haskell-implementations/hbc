module PreludeX where
-- XXX specialize
fromIntegral	:: (Integral a, Num b) => a -> b
fromIntegral	=  fromInteger . toInteger
