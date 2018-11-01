module P_List_zipWith4  where
zipWith4		:: (a->b->c->d->e) -> [a]->[b]->[c]->[d]->[e]
zipWith4 z (a:as) (b:bs) (c:cs) (d:ds)
			=  z a b c d : zipWith4 z as bs cs ds
zipWith4 _ _ _ _ _	=  []
