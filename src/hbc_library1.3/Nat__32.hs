#include "bit.h"
module Nat__32(Nat32(..)) where
import LMLbitops

newtype Nat32 = Nat32 Int deriving (Eq, Ord)

instance Num Nat32 where
	Nat32 x + Nat32 y = Nat32 (MASK32(x+y))
	Nat32 x - Nat32 y = Nat32 (MASK32(x-y))
	Nat32 x * Nat32 y = Nat32 (MASK32(x*y))
	negate (Nat32 x) = Nat32 (MASK32(negate x))
	fromInteger x   = if x < 0 then
				error "Nat32.fromInteger: < 0"
			  else
			        Nat32 (MASK32(fromInteger x))
	fromInt x       = Nat32 (MASK32(x))

instance Bounded Nat32 where
	minBound = Nat32 0
	maxBound = Nat32 0xffffffff

instance Enum Nat32 where
	toEnum n = Nat32 (MASK32(n))
	fromEnum (Nat32 n) = n
	enumFrom n = enumFromTo n maxBound
	enumFromThen n m = enumFromThenTo n m (if m > n then maxBound else minBound)

instance Real Nat32 where
	toRational (Nat32 n) = toRational (TOUNSIGNED32(n))

instance Integral Nat32 where
	toInteger (Nat32 n) = toInteger n
	toInt (Nat32 n) = n
	-- Must handle negative numbers correctly, so convert to Integer
	quotRem (Nat32 x) (Nat32 y) = (Nat32 (FROMINTEGER32(q)), Nat32 (FROMINTEGER32(r))) 
		where (q, r) = quotRem (TOUNSIGNED32(x)) (TOUNSIGNED32(y))
	divMod  (Nat32 x) (Nat32 y) = (Nat32 (FROMINTEGER32(q)), Nat32 (FROMINTEGER32(r))) 
		where (q, r) = divMod  (TOUNSIGNED32(x)) (TOUNSIGNED32(y))

instance Show Nat32 where
	showsPrec p (Nat32 w) = showsPrec p (TOUNSIGNED32(w))
	showsType _ = showString "Nat32"

instance Read Nat32 where
	readsPrec p s = [(Nat32 (FROMINTEGER32(n)), s) | (n, s) <- readsPrec p s]



