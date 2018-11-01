module PreludeX where
(!!)			:: [b] -> Int -> b
_      !! n | n < 0     =  error "Prelude.!!: negative index"
xs     !! n             =  sub xs n
	   where sub :: [a] -> Int -> a
                 sub []     _ = error "Prelude.!!: index too large"
		 sub (x:_)  0 = x
                 sub (_:xs) n = sub xs (n-1)
