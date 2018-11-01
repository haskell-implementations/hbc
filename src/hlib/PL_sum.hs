module P_List_sum(sum)  where
{-# SPECIALIZE sum :: [Int] -> Int, [Integer] -> Integer, [Double] -> Double, [Float] -> Float, [Complex Float] -> Complex Float, [Complex Double] -> Complex Double #-}
sum		:: (Num a) => [a] -> a
sum	l	= sum' l 0
sum' []     a = a
sum' (x:xs) a = sum' xs (a+x)
