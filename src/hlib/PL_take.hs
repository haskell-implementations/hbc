module P_List_take  where
-- SLOW
{-# SPECIALIZE take :: Int -> [b] -> [b], Integer -> [b] -> [b] #-}
take			:: (Integral a) => a -> [b] -> [b]
take   n   xs | n >= 0  = itake (toInt n) xs
itake			:: Int -> [b] -> [b]
itake  0     _		=  []
itake  _     []		=  []
itake  n     (x:xs)	=  x : itake (n-1) xs
