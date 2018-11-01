module PreludeX where
init			:: [a] -> [a]
init []			=  error "PreludeList.init: empty list"
init [x]		=  []
init (x:xs)		=  x : init xs
