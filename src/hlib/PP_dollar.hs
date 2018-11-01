module P_Prelude_dollar(($)) where
-- infixr 0  $
-- right-associating infix application operator (useful in continuation-
-- passing style)
($)                   :: (a -> b) -> a -> b
f $ x                 =  f x
