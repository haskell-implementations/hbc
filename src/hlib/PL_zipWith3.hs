module P_List_zipWith3  where
zipWith3		:: (a->b->c->d) -> [a]->[b]->[c]->[d]
zipWith3 z (a:as) (b:bs) (c:cs)
			=  z a b c : zipWith3 z as bs cs
zipWith3 _ _ _ _	=  []
