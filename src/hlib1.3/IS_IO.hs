module PreludeX where
instance (Show a) => Show (IO a) where
	showsType x = showString "(IO " . showsType (f x) . showChar ')'
		where f :: (IO a) -> a
		      f _ = error "showsType eval IO a"

