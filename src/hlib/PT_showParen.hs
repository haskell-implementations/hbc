module	P_Text_showParen(showParen) where
showParen   	:: Bool -> ShowS -> ShowS
showParen b p s =  if b then (showChar '(' . p . showChar ')') s else p s
