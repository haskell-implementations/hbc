#define intArray {-:"PintArray":-}
#define intIndex {-:"PintIndex":-}
#define outside {-:"Poutside":-}

module 	Array(
	Array, array, listArray, (!), bounds, indices, elems, 
	assocs, accumArray, (//), accum, ixmap,
	module Ix,
	outside) where

import List((\\))
import Ix
import LML_array
import _LibArray
import Array__array
import Array__bang

#define lmlsarray {-:"Psarray":-}
#define lmlindex {-:"Paindex":-}
#define lmlarray {-:"Parray":-}

infixl 9 //

instance  (Ix a, Eq b)  => Eq (Array a b)  where
    a == a'  	        =  assocs a == assocs a'		-- SLOW!!!

instance  (Ix a, Ord b) => Ord (Array a b)  where
    a <=  a'  	    	=  assocs a <=  assocs a'		-- SLOW!!!

instance  (Ix a, Show a, Show b) => Show (Array a b)  where
    showsPrec p a = showParen (p > 9) (
		    showString "array " .
		    shows (bounds a) . showChar ' ' .
		    shows (assocs a)                  )
    showsType x = showString "(Array " . showsType (f x) . showString " " . showsType (g x) . showChar ')'
		where f :: (Ix a) => (Array a b) -> a
		      f _ = error "showsType eval array index"
		      g :: (Ix a) => (Array a b) -> b
		      g _ = error "showsType eval array value"

instance  (Ix a, Read a, Read b) => Read (Array a b)  where
    readsPrec p = readParen (p > 9)
	   (\r -> [(array b as, u) | ("array",s) <- lex r,
				     (b,t)       <- reads s,
				     (as,u)      <- reads t   ]
		  ++
		  [(listArray b xs, u) | ("listArray",s) <- lex r,
					 (b,t)           <- reads s,
					 (xs,u)          <- reads t ])

{-# SPECIALIZE listArray :: (Int,Int) -> [a] -> Array Int a #-}
listArray	:: (Ix a) => (a,a) -> [b] -> Array a b
listArray b@(l,u) vs = 
	let hi = index b u
	    fla :: Int -> Int -> [a] -> [(Int,a)]
	    fla _ _ [] = []
	    fla i m (x:xs) = if i>m then [] else (i,x) : fla (i+1) m xs
	in  MkArray b (lmlsarray 0 (hi `max` -1) 0 (fla 0 hi vs))
-- !!! is this correct?

{-# SPECIALIZE bounds :: Array Int a -> (Int,Int) #-}
bounds		:: (Ix a) => Array a b -> (a,a)
bounds (MkArray b _)  = b

indices		:: (Ix a) => Array a b -> [a]
indices (MkArray b _) = range b

#define rawindex {-:"Prindexu":-}

{-# SPECIALIZE elems :: Array Int a -> [a] #-}
elems		:: (Ix a) => Array a b -> [b]
elems (MkArray b@(l,u) a) = fe a 0 ((index b u){-#STRICT#-})
				where fe :: LArray a -> Int -> Int -> [a]
				      fe a n m = if n > (m{-#NOEVAL#-}) then [] else ((rawindex a n){-#STRICT#-}) : fe a (n+1) m

assocs :: (Ix a) => Array a b -> [(a, b)]
assocs a = [(i, a!i) | i <- indices a]

accumArray :: (Ix a) => (b -> c -> b) -> b -> (a,a) -> [(a, c)] -> Array a b
accumArray f z b@(l,u) ivs  = 
	MkArray b (lmlarray 0 (index b u `max` -1) (foldl f z) [((if inRange b i then index b i else error "Array.accumArray: index out of range"), v) | (i, v) <- ivs])

{-# SPECIALIZE (//) :: Array Int b -> [(Int, b)] -> Array Int b #-}
(//) :: (Ix a) => Array a b -> [(a, b)] -> Array a b
a // us	= array (bounds a)
		([(i, a!i) | i <- indices a \\ [i | (i, _) <- us]] ++ us)

accum :: (Ix a) => (b -> c -> b) -> Array a b -> [(a, c)] -> Array a b
-- SLOW
accum f a l = foldl (\a (i, v) -> a // [(i, f (a!i) v)]) a l

amap :: (Ix a) => (b -> c) -> Array a b -> Array a c
amap f a = array b [(i, f (a!i)) | i <- range b]
	where b = bounds a

instance (Ix a) => Functor (Array a) where
    map f a = amap f a


ixmap :: (Ix a, Ix b) => (a,a) -> (a -> b) -> Array b c -> Array a c
ixmap b f a           = array b [(i, a ! f i) | i <- range b]

-----------

#define rindex {-:"Prindex":-}
#define veksize {-:"Pord":-}

intArray :: (Int,Int) -> [(Int, b)] -> Array Int b
intArray b@(l,u) ivs  = 
	MkArray b (lmlsarray 0 ((u-l) `max` -1) l ivs)	-- this evaluates l and u

intIndex :: Array Int a -> Int -> a
intIndex (MkArray (l,u) a) i = 
	if i < (l{-#NOEVAL#-}) || i > (u{-#NOEVAL#-}) then
	    outside
	else
	    rindex a (i-(l{-#NOEVAL#-}))

outside = error "Prelude.! indexing out of range"
