#include "bit.h"
module Nat__8(Nat8(..)) where
import LMLbitops

newtype Nat8 = Nat8 Int deriving (Eq, Ord)

instance Num Nat8 where
	Nat8 x + Nat8 y = Nat8 (MASK8(x+y))
	Nat8 x - Nat8 y = Nat8 (MASK8(x-y))
	Nat8 x * Nat8 y = Nat8 (MASK8(x*y))
	negate (Nat8 x) = Nat8 (MASK8(negate x))
	fromInteger x   = if x < 0 then
				error "Nat8.fromInteger: < 0"
			  else
			        Nat8 (MASK8(fromInteger x))
	fromInt x       = Nat8 (MASK8(x))

instance Bounded Nat8 where
	minBound = Nat8 0
	maxBound = Nat8 0xff

instance Enum Nat8 where
	toEnum n = Nat8 (MASK8(n))
	fromEnum (Nat8 n) = n
	enumFrom n = enumFromTo n maxBound
	enumFromThen n m = enumFromThenTo n m (if m > n then maxBound else minBound)

instance Real Nat8 where
	toRational (Nat8 n) = toRational n

instance Integral Nat8 where
	toInteger (Nat8 n) = toInteger n
	toInt (Nat8 n) = n
	quotRem (Nat8 x) (Nat8 y) = (Nat8 q, Nat8 r) where (q, r) = quotRem x y
	divMod  (Nat8 x) (Nat8 y) = (Nat8 q, Nat8 r) where (q, r) = divMod  x y

instance Show Nat8 where
	showsPrec p (Nat8 w) = showsPrec p w
	showsType _ = showString "Nat8"

instance Read Nat8 where
	readsPrec p s = [(Nat8 n, s) | (n, s) <- readsPrec p s]
