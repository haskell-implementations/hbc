module PreludeX where
seq :: (Eval a) => a -> b -> b
seq x y = {-:"Pseq":-} x y
