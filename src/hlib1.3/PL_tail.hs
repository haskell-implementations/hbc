module PreludeX where
tail			:: [a] -> [a]
tail (_:xs)		=  xs
tail []			=  error "PreludeList.tail: empty list"
