module PreludeX where
drop			:: Int -> [a] -> [a]
drop  0     xs		=  xs
drop  _     []		=  []
drop  n     xs		=  if n > 0 then drop' n xs else error "PreludeList.drop: negative argument"
	where drop' :: Int -> [a] -> [a]
	      drop' _ [] = []
	      drop' n l@(x:xs) = if n > 0 then drop' (n-1) xs else l
