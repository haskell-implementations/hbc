module PreludeX where
take			:: Int -> [a] -> [a]
take  0     _		=  []
take  _     []		=  []
take  n     xs		=  if n > 0 then take' n xs else error "PreludeList.take: negative argument"
	where take' :: Int -> [a] -> [a]
	      take' _ [] = []
	      take' n (x:xs) = if n > 0 then x : take' (n-1) xs else []
