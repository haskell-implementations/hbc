module P_List_product(product)  where
{-# SPECIALIZE product :: [Int] -> Int, [Integer] -> Integer, [Double] -> Double, [Float] -> Float #-}
product		:: (Num a) => [a] -> a
product	l	= prod l 1
prod []     a = a
prod (x:xs) a = prod xs (a*x)
