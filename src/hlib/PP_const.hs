module P_Prelude_const(const) where
-- constant function
const                 :: a -> b -> a
const x _             =  x
