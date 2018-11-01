module PreludeX where
{-# SPECIALIZE fromRealFrac :: Float -> Double, Double -> Float #-}
fromRealFrac	:: (RealFrac a, Fractional b) => a -> b
fromRealFrac	=  fromRational . toRational
