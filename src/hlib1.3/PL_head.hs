module PreludeX where
head			:: [a] -> a
head (x:_)		=  x
head []			=  error "PreludeList.head: empty list"
