module PreludeX where
uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f p = f x y where (x, y) = p
