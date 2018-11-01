#include "bit.h"
module Nat__64(Nat64(..)) where
import LMLbitops

newtype Nat64 = Nat64 INT64 deriving (Eq, Ord)

instance Num Nat64 where
	Nat64 x + Nat64 y = Nat64 (MASK64(x+y))
	Nat64 x - Nat64 y = Nat64 (MASK64(x-y))
	Nat64 x * Nat64 y = Nat64 (MASK64(x*y))
	negate (Nat64 x) = Nat64 (MASK64(negate x))
	fromInteger x   = if x < 0 then
				error "Nat64.fromInteger: < 0"
			  else
			        Nat64 (MASK64(fromInteger x))
	fromInt x       = Nat64 (MASK64(fromInt x))

instance Bounded Nat64 where
	minBound = Nat64 0
	maxBound = Nat64 0xffffffffffffffff

instance Enum Nat64 where
	toEnum n = Nat64 (fromInt n)
	fromEnum (Nat64 n) = toInt n
	enumFrom n = enumFromTo n maxBound
	enumFromThen n m = enumFromThenTo n m (if m > n then maxBound else minBound)

instance Real Nat64 where
	toRational (Nat64 n) = toRational (TOUNSIGNED64(n))

instance Integral Nat64 where
	toInteger (Nat64 n) = toInteger n
	toInt (Nat64 n) = toInt n
	-- Must handle negative numbers correctly, so convert to Integer
	quotRem (Nat64 x) (Nat64 y) = (Nat64 (fromInteger q), Nat64 (fromInteger r)) 
		where (q, r) = quotRem (TOUNSIGNED64(x)) (TOUNSIGNED64(y))
	divMod  (Nat64 x) (Nat64 y) = (Nat64 (fromInteger q), Nat64 (fromInteger r)) 
		where (q, r) = divMod  (TOUNSIGNED64(x)) (TOUNSIGNED64(y))

instance Show Nat64 where
	showsPrec p (Nat64 w) = showsPrec p (TOUNSIGNED64(w))
	showsType _ = showString "Nat64"

instance Read Nat64 where
	readsPrec p s = [(Nat64 (fromInteger n), s) | (n, s) <- readsPrec p s]
