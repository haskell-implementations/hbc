module PreludeX where
instance (Show a, Show b) => Show (a -> b) where
	showsType x = showChar '(' . showsType (f x) . showString " -> " . showsType (g x) . showChar ')'
		where f :: (a -> b) -> a
		      f _ = error "showsType eval a->"
		      g :: (a -> b) -> b
		      g _ = error "showsType eval ->b"
