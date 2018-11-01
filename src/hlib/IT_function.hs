module I_Text_function where
instance (Text a, Text b) => Text (a -> b) where
        readsPrec p s = error "Cannot read functions"
	showsType x = showChar '(' . showsType (f x) . showString " -> " . showsType (g x) . showChar ')'
		where f :: (a -> b) -> a
		      f _ = error "showsType eval a->"
		      g :: (a -> b) -> b
		      g _ = error "showsType eval ->b"
