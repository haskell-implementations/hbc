module I_Text_List where
instance  (Text a) => Text [a]  where
    readsPrec p = readList
    showsPrec p = showList
    showsType x = showChar '[' . showsType (f x) . showChar ']'
		where f :: [a] -> a
		      f _ = error "showsType eval []"
