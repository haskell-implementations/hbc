module PreludeX where
curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)
