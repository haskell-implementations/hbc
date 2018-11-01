module PreludeX where
last			:: [a] -> a
last []			=  error "PreludeList.last: empty list"
last [x]		=  x
last (_:xs)		=  last xs
