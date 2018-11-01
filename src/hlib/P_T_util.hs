module P_Text_util where
readDec, readOct, readHex :: (Num a) => ReadS a
readDec = readInt 10 isDigit (\d -> ord d - ord '0')
readOct = readInt  8 isOctDigit (\d -> ord d - ord '0')
readHex = readInt 16 isHexDigit hex
	    where hex d = ord d - (if isDigit d then ord '0'
				   else ord (if isUpper d then 'A' else 'a')
					- 10)

readInt :: (Num a) => a -> (Char -> Bool) -> (Char -> Int) -> ReadS a
readInt radix isDig digToInt s =
    [(foldl1 (\n d -> n * radix + d) (map (fromInt . digToInt) ds), r)
	| (ds,r) <- nonnull isDig s ]

showInt	:: (Integral a) => a -> ShowS
showInt n r = let (n',d) = quotRem n 10
		  r' = chr (ord '0' + fromIntegral d) : r
	      in if n' == 0 then r' else showInt n' r'

isOctDigit c  =  c >= '0' && c <= '7'
isHexDigit c  =  isDigit c || c >= 'A' && c <= 'F'
			   || c >= 'a' && c <= 'f'

nonnull			:: (Char -> Bool) -> ReadS String
nonnull p s		=  [(cs,t) | (cs@(_:_),t) <- [span p s]]

readSigned:: (Num a) => ReadS a -> ReadS a
readSigned readPos = readParen False read'
		     where read' r  = read'' r ++
				      [(-x,t) | ("-",s) <- lex r,
						(x,t)   <- read'' s]
			   read'' r = [(n,s)  | (str,s) <- lex r,
		      				(n,"")  <- readPos str]

showSigned:: (Num a, Ord a) => (a -> ShowS) -> Int -> a -> ShowS
showSigned showPos p x = if x < 0 then showParen (p > 6)
						 (showChar '-' . showPos (-x))
				  else showPos x

