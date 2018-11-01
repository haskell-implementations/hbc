module PreludeX where
notElemBy		:: (a -> a -> Bool) -> a -> [a] -> Bool
notElemBy eq x []	=  True
notElemBy eq x (y:ys)   =  not (eq x y) && notElemBy eq x ys
