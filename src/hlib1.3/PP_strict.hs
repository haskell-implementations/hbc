module PreludeX where
strict :: (Eval a) => (a -> b) -> (a -> b)
strict f x = x `seq` f x
