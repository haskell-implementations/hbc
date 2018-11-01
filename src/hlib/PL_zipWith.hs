module P_List_zipWith  where
zipWith			:: (a->b->c) -> [a]->[b]->[c]
zipWith z (a:as) (b:bs)	=  z a b : zipWith z as bs
zipWith _ _ _		=  []
