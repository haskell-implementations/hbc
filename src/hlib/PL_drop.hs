module P_List_drop(drop)  where
{-# SPECIALIZE drop :: Int -> [b] -> [b], Integer -> [b] -> [b] #-}
drop			:: (Integral a) => a -> [b] -> [b]
drop  n     xs | n >= 0 = idrop (toInt n) xs
idrop			:: Int -> [b] -> [b]
idrop  0     xs		=  xs
idrop  _     []		=  []
idrop  n     (_:xs)	=  idrop (n-1) xs
