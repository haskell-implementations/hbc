#define intArray {-:"PintArray":-}
#define intIndex {-:"PintIndex":-}
#define outside {-:"Poutside":-}

module  PreludeArrayExtra (intArray, intIndex, outside) where
-- Extra stuff for fast Int indexing

import LML_array
#define lmlsarray {-:"Psarray":-}
#define rindex {-:"Prindex":-}
#define veksize {-:"Pord":-}
#define lmlindex {-:"Paindex":-}

import P_Array_data
{-
-- Taken from P_Array.hs
--data  Assoc a b =  a := b	deriving (Eq, Ord, Text, Binary)
type Assoc a b = (a,b)
data  (Ix a)    => Array a b = MkArray (a,a){-#STRICT#-} (LArray b){-#STRICT#-} deriving ()
-}

intArray :: (Int,Int) -> [Assoc Int b] -> Array Int b
intArray b@(l,u) ivs  = 
	MkArray b (lmlsarray 0 ((u-l) `max` -1) l [(i,v) | (i:=v) <- ivs])	-- this evaluates l and u

intIndex :: Array Int a -> Int -> a
intIndex (MkArray (l,u) a) i = 
#if 0
	lmlindex a (i-(l{-#NOEVAL#-}))
#else
	if i < (l{-#NOEVAL#-}) || i > (u{-#NOEVAL#-}) then
	    outside
	else
	    rindex a (i-(l{-#NOEVAL#-}))
#endif

outside = error "Prelude.(!) indexing out of range"
