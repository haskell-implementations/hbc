module P_Prelude_asTypeOf(asTypeOf) where
asTypeOf		:: a -> a -> a
x `asTypeOf` _ 		=  x
