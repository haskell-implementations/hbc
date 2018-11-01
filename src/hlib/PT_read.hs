module	P_Text_read (read) where
read 	    	:: (Text a) => String -> a
read s 	    	=  case [x | (x,t) <- reads s, ("","") <- lex t] of
			[x] -> x
			[]  -> error "Prelude.read: no parse"
			_   -> error "Prelude.read: ambiguous parse"
