module P_List_conc  where
infixr 5  ++
(++)			:: [a] -> [a] -> [a]
[] ++ ys		=  ys
(x:xs) ++ ys		=  x : (xs ++ ys)
