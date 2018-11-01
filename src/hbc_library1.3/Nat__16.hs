#include "bit.h"
module Nat__16(Nat16(..)) where
import LMLbitops

newtype Nat16 = Nat16 Int deriving (Eq, Ord)

instance Num Nat16 where
	Nat16 x + Nat16 y = Nat16 (MASK16(x+y))
	Nat16 x - Nat16 y = Nat16 (MASK16(x-y))
	Nat16 x * Nat16 y = Nat16 (MASK16(x*y))
	negate (Nat16 x)  = Nat16 (MASK16(negate x))
	fromInteger x     = if x < 0 then
				error "Nat16.fromInteger: < 0"
			  else
			        Nat16 (MASK16(fromInteger x))
	fromInt x         = Nat16 (MASK16(x))

instance Bounded Nat16 where
	minBound = Nat16 0
	maxBound = Nat16 0xffff

instance Enum Nat16 where
	toEnum n = Nat16 (MASK16(n))
	fromEnum (Nat16 n) = n
	enumFrom n = enumFromTo n maxBound
	enumFromThen n m = enumFromThenTo n m (if m > n then maxBound else minBound)

instance Real Nat16 where
	toRational (Nat16 n) = toRational n

instance Integral Nat16 where
	toInteger (Nat16 n) = toInteger n
	toInt (Nat16 n) = n
	quotRem (Nat16 x) (Nat16 y) = (Nat16 q, Nat16 r) where (q, r) = quotRem x y
	divMod  (Nat16 x) (Nat16 y) = (Nat16 q, Nat16 r) where (q, r) = divMod  x y

instance Show Nat16 where
	showsPrec p (Nat16 w) = showsPrec p w
	showsType _ = showString "Nat16"

instance Read Nat16 where
	readsPrec p s = [(Nat16 n, s) | (n, s) <- readsPrec p s]
