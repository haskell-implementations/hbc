module PreludeX where
{-# SPECIALIZE gcd :: Int -> Int -> Int, Integer -> Integer -> Integer #-}
gcd		:: (Integral a) => a -> a -> a
gcd 0 0		=  error "Prelude.gcd: gcd 0 0 is undefined."
gcd x y		=  gcd' (abs x) (abs y)
		   where gcd' x 0  =  x
			 gcd' x y  =  gcd' y (x `rem` y)
