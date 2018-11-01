module P_List_sub  where
infixl 9  !!
{-# SPECIALIZE (!!) :: [b] -> Int -> b, [b] -> Integer -> b #-}
(!!)			:: (Integral a) => [b] -> a -> b
{-
(x:_)  !! 0		=  x
(_:xs) !! (n+1)		=  xs !! n
(_:_)  !! _		=  error "(!!){PreludeList}: negative index"
[]     !! (_+1)		=  error "(!!){PreludeList}: index too large"
-}
-- The semantics is not quite the same for error conditions in the more efficient version.
_      !! n | n < 0     =  error "Prelude.(!!): negative index"
xs     !! n             =  sub xs (toInt n)
			   where sub :: [a] -> Int -> a
				 sub (x:_)  0 = x
                                 sub (_:xs) n = sub xs (n-1)
                                 sub []     _ = error "Prelude.(!!): index too large"
