module P_List_nub(nub)  where
nub			:: (Eq a) => [a] -> [a]
nub l			= nub' l []
nub' [] _		= []
nub' (x:xs) l		= if x `elem` l then nub' xs l else x : nub' xs (x:l)
