module	C_Text(Text(..)) where
class  Text a  where
    readsPrec :: Int -> ReadS a
    showsPrec :: Int -> a -> ShowS
    readList  :: ReadS [a]
    showList  :: [a] -> ShowS
    showsType :: a -> ShowS

    readsPrec _ _ = let v = error ("Prelude.Text.readsPrec: Cannot read type "++showsType (f v) ".") in v
		    where f :: [(a,String)] -> a
			  f _ = error "readsPrec-eval"
    showsPrec _ x = showString "<<" . showsType x . showString ">>"

    readList    = readParen False (\r -> [pr | ("[",s)	<- lex r,
					       pr	<- readl s])
	          where readl  s = [([],t)   | ("]",t)  <- lex s] ++
				   [(x:xs,u) | (x,t)    <- reads s,
					       (xs,u)   <- readl' t]
			readl' s = [([],t)   | ("]",t)  <- lex s] ++
			           [(x:xs,v) | (",",t)  <- lex s,
					       (x,u)	<- reads t,
					       (xs,v)   <- readl' u]

    showList []	= showString "[]"
    showList (x:xs)
		= showChar '[' . shows x . showl xs
		  where showl []     = showChar ']'
			showl (x:xs) = showString ", " . shows x . showl xs

shows x s	=  showsPrec 0 x s

reads 	        :: (Text a) => ReadS a
reads s		=  readsPrec 0 s

read 	    	:: (Text a) => String -> a
read s 	    	=  case [x | (x,t) <- reads s, ("","") <- lex t] of
			[x] -> x
			[]  -> error "Prelude.Text.read: no parse"
			_   -> error "Prelude.Text.read: ambiguous parse"

