module P_Prelude_compose((.)) where
(.)			:: (b -> c) -> (a -> b) -> a -> c
(f . g) x		=  f (g x)
