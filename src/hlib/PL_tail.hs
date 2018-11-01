module P_List_tail  where
tail			:: [a] -> [a]
tail (_:xs)		=  xs
tail []			=  error "Prelude.tail: tail []"
