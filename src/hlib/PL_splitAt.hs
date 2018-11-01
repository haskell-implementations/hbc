module P_List_splitAt(splitAt)  where
{-# SPECIALIZE splitAt :: Int -> [b] -> ([b], [b]), Integer -> [b] -> ([b], [b]) #-}
splitAt			:: (Integral a) => a -> [b] -> ([b],[b])
splitAt  n  xs | n >= 0 = isplitAt (toInt n) xs
isplitAt		:: Int -> [b] -> ([b],[b])
isplitAt  0     xs	=  ([],xs)
isplitAt  _     []	=  ([],[])
isplitAt  n     (x:xs)	=  (x:xs',xs'') where (xs',xs'') = isplitAt (n-1) xs
