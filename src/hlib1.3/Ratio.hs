module Ratio (
	Ratio, Rational(..), (%), numerator, denominator, approxRational) where

import Ratio__approxRational

infixl 7  %, :%

prec = 7::Int

data  (Integral a)	=> Ratio a = !a :% !a
type  Rational		=  Ratio Integer

(%)			:: (Integral a) => a -> a -> Ratio a
numerator, denominator	:: (Integral a) => Ratio a -> a


reduce x 0              =  signum x :% 0
reduce x y		=  (x `quot` d) :% (y `quot` d)
			   where d = gcd x y


x % y			=  reduce (x * signum y) (abs y)

numerator (x:%y)	=  x

denominator (x:%y)	=  y

{-# SPECIALIZE instance Eq (Ratio Integer) #-}
instance  (Integral a)	=> Eq (Ratio a)  where
    (x:%y) == (x':%y')	=  x == x' && y == y'
    (x:%y) /= (x':%y')	=  x /= x' || y /= y'

{-# SPECIALIZE instance Ord (Ratio Integer) #-}
instance  (Integral a)	=> Ord (Ratio a)  where
    (x:%y) <= (x':%y')	=  x * y' <= x' * y
    (x:%y) <  (x':%y')	=  x * y' <  x' * y

{-# SPECIALIZE instance Num (Ratio Integer) #-}
instance  (Integral a)	=> Num (Ratio a)  where
    (x:%y) + (x':%y')	=  reduce (x*y' + x'*y) (y*y')
    (x:%y) - (x':%y')	=  reduce (x*y' - x'*y) (y*y')
    (x:%y) * (x':%y')	=  reduce (x * x') (y * y')
    negate (x:%y)	=  (-x) :% y
    abs (x:%y)		=  abs x :% y
    signum (x:%y)	=  signum x :% 1
    fromInteger x	=  fromInteger x :% 1
    {-:"PfromInteger":-} x = case x{-# NOEVAL #-} of { (i,_,_) -> fromInteger (i{-# NOEVAL #-}) :% 1 }

{-# SPECIALIZE instance Real (Ratio Integer) #-}
instance  (Integral a)	=> Real (Ratio a)  where
    toRational (x:%y)	=  toInteger x :% toInteger y

{-# SPECIALIZE instance Fractional (Ratio Integer) #-}
instance  (Integral a)	=> Fractional (Ratio a)  where
    (x:%y) / (x':%y')	=  (x*y') % (y*x')
    recip (x:%y)	=  if x < 0 then (-y) :% (-x) else y :% x
    fromRational (x:%y) =  fromInteger x :% fromInteger y
    {-:"PfromRational":-} x = case x{-# NOEVAL #-} of { (r,_) -> case r{-# NOEVAL #-} of { (x:%y) -> fromInteger x :% fromInteger y } }

{-# SPECIALIZE instance RealFrac (Ratio Integer) #-}
instance  (Integral a)	=> RealFrac (Ratio a)  where
    properFraction (x:%y) = (fromIntegral q, r:%y)
			    where (q,r) = quotRem x y

{-# SPECIALIZE instance Enum (Ratio Integer) #-}
instance  (Integral a) => Enum (Ratio a) where
	enumFrom x = enumFromBy x 1
	enumFromThen x y = enumFromBy x (y - x)
	toEnum n = fromInteger (toInteger n)
	fromEnum n = error "Ratio.fromEnum: can't use fromEnum with Ratio"

enumFromBy n k		=  n : enumFromBy (n+k) k

instance  (Integral a, Show a) => Show (Ratio a)  where
    showsPrec p (x:%y)	=  showParen (p > prec)
    	    	    	       (shows x . showString " % " . shows y)

    showsType ~(x:%y) = showString "(Ratio " . showsType x . showChar ')'

instance  (Integral a, Read a) => Read (Ratio a)  where
    readsPrec p  =  readParen (p > prec)
			      (\r -> [(x%y,u) | (x,s)   <- reads r,
					        ("%",t) <- lex s,
						(y,u)   <- reads t ])

