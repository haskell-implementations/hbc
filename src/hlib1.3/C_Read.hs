module	PreludeX(Read(..)) where
class  Read a  where
    readsPrec :: Int -> ReadS a
    readList  :: ReadS [a]

#if 0
    readsPrec _ _ = let v = error ("Prelude.readsPrec: Cannot read type "++showsType (f v) ".") in v
		    where f :: [(a,String)] -> a
			  f _ = error "readsPrec-eval"

#else
    readsPrec _ _ = error ("Prelude.readsPrec: No default")
#endif
    readList    = readParen False (\r -> [pr | ("[",s)	<- lex r,
					       pr	<- readl s])
	          where readl  s = [([],t)   | ("]",t)  <- lex s] ++
				   [(x:xs,u) | (x,t)    <- reads s,
					       (xs,u)   <- readl' t]
			readl' s = [([],t)   | ("]",t)  <- lex s] ++
			           [(x:xs,v) | (",",t)  <- lex s,
					       (x,u)	<- reads t,
					       (xs,v)   <- readl' u]

reads 	        :: (Read a) => ReadS a
reads s		=  readsPrec 0 s

read 	    	:: (Read a) => String -> a
read s 	    	=  case [x | (x,t) <- reads s, ("","") <- lex t] of
			[x] -> x
			[]  -> error "Prelude.read: no parse"
			_   -> error "Prelude.read: ambiguous parse"

