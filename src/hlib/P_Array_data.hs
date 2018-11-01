module PreludeCore where
import LML_array

infix  5  :=

data Assoc a b =  a := b	deriving (Eq, Ord, Text, Binary)

data (Ix a) => Array a b = 
	MkArray (a,a){-#STRICT#-} (LArray b){-#STRICT#-} deriving ()

instance  (Ix a, Eq b)  => Eq (Array a b)  where
    a == a'  	        =  assocs a == assocs a'		-- SLOW!!!

instance  (Ix a, Ord b) => Ord (Array a b)  where
    a <=  a'  	    	=  assocs a <=  assocs a'		-- SLOW!!!

instance  (Ix a, Text a, Text b) => Text (Array a b)  where
    showsPrec p a = showParen (p > 9) (
		    showString "array " .
		    shows (bounds a) . showChar ' ' .
		    shows (assocs a)                  )

    readsPrec p = readParen (p > 9)
	   (\r -> [(array b as, u) | ("array",s) <- lex r,
				     (b,t)       <- reads s,
				     (as,u)      <- reads t   ]
		  ++
		  [(listArray b xs, u) | ("listArray",s) <- lex r,
					 (b,t)           <- reads s,
					 (xs,u)          <- reads t ])

    showsType x = showString "(Array " . showsType (f x) . showString " " . showsType (g x) . showChar ')'
		where f :: (Ix a) => (Array a b) -> a
		      f _ = error "showsType eval array index"
		      g :: (Ix a) => (Array a b) -> b
		      g _ = error "showsType eval array value"

instance  (Ix a, Binary a, Binary b) => Binary (Array a b)  where
    showBin a r = {-:"PshowBin":-} a r
    readBin b = {-:"PreadBin":-} b
{-
    showBin a = showBin (bounds a) . showBin (elems a)

    readBin bin = (listArray b vs, bin'')
		 where (b,bin')   = readBin bin
		       (vs,bin'') = readBin bin'
-}
