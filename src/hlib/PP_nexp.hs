module P_Prelude_nexp((^)) where
{-# SPECIALIZE (^) :: Int -> Int -> Int, Integer -> Int -> Integer, Double -> Int -> Double, Float -> Int -> Float, Complex Float -> Int -> Complex Float, Complex Double -> Int -> Complex Double #-}
(^)		:: (Num a, Integral b) => a -> b -> a
x ^ 0		=  1
x ^ (n+1)	=  f x n x
		   where f _ 0 y = y
		         f x n y = g x n  where
			           g x n | odd n     = f x (n-1) (x*y)
				         | otherwise = g (x*x) (n`div`2)
_ ^ _		= error "Prelude.(^): negative exponent"
