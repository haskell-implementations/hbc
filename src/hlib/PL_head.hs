module P_List_head  where
head			:: [a] -> a
head (x:_)		=  x
head []			=  error "Prelude.head: head []"
