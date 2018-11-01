module	P_Text_showChar(showChar) where
showChar    	:: Char -> ShowS
showChar c cs  	=  c : cs
