module PreludeX where
{-# SPECIALIZE (^^) :: Float -> Int -> Float, Double -> Int -> Double {-, Rational -> Int -> Rational-} #-}
-- MORE!!
(^^)		:: (Fractional a, Integral b) => a -> b -> a
x ^^ n		=  if n >= 0 then x^n else recip(x^(-n))
