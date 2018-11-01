module P_List_last  where
last			:: [a] -> a
last []			=  error "Prelude.last: last []"
last [x]		=  x
last (_:xs)		=  last xs
