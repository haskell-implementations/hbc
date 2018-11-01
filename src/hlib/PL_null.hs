module P_List_null  where
null			:: [a] -> Bool
null []			=  True
null (_:_)		=  False
