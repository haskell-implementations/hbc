module PreludeX where
instance  (Show a) => Show [a]  where
    showsPrec p = showList
    showsType x = showChar '[' . showsType (f x) . showChar ']'
		where f :: [a] -> a
		      f _ = error "showsType eval []"
