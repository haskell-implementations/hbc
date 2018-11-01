module P_List_init  where
init			:: [a] -> [a]
init []			=  error "Prelude.init: init []"
init [x]		=  []
init (x:xs)		=  x : init xs
