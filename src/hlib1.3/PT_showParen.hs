module PreludeX where
showParen   	:: Bool -> ShowS -> ShowS
showParen b p s =  if b then (showChar '(' . p . showChar ')') s else p s
