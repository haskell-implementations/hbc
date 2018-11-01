module P_Prelude_flip(flip) where
flip			:: (a -> b -> c) -> b -> a -> c
flip f x y		=  f y x
