module PreludeX where
{-# SPECIALIZE product :: [Int] -> Int, [Integer] -> Integer, [Double] -> Double, [Float] -> Float #-}
product		:: (Num a) => [a] -> a
product	l	= prod l 1
	where prod []     a = a
	      prod (x:xs) a = prod xs (a*x)
