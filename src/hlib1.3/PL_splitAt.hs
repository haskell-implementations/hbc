module PreludeX where
splitAt			:: Int -> [b] -> ([b],[b])
splitAt  n  xs | n >= 0 = isplitAt n xs
  where isplitAt		:: Int -> [b] -> ([b],[b])
  	isplitAt  0     xs	=  ([],xs)
	isplitAt  _     []	=  ([],[])
	isplitAt  n     (x:xs)	=  (x:xs',xs'') where (xs',xs'') = isplitAt (n-1) xs
splitAt  _  _           = error "PreludeList.splitAt: negative argument"
