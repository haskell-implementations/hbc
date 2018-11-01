#include "bit.h"
module Bit ( Bits(..) ) where

import Nat__8
import Nat__16
import Nat__32
import Nat__64
import LMLbitops
import LMLintegerMisc

infixl 8 ^<<, ^>>, `asr`, `rol`, `ror`
infixl 7 ^&
infixl 6 ^|
infixl 5 `xor`

class (Integral a) => Bits a where
	(^&), (^|), xor		:: a -> a -> a
	compl		 	:: a -> a
	(^>>), (^<<), asr, rol, ror :: a -> Int -> a
	test			:: Int -> a -> Bool
	set, clear		:: Int -> a -> a

	bitSize	 		:: a -> Int

        a ^| b =    compl (compl a ^& compl b)

	a `xor` b = (a ^& b) ^| (compl a ^& compl b)

        w ^<< n
	  | n < 0 || n >= bitSize w = 0
 	  | otherwise =
		    foldr set 0
                      [ i+n | i <- [0..bitSize w - (n+1)],
                              test i w ]

        w ^>> n
	  | n < 0 || n >= bitSize w = 0
 	  | otherwise =
		    foldr set 0
                      [ i-n | i <- [n..bitSize w - 1],
                              test i w ]

        w `asr` n
  	  | n < 0 || n >= bitSize w = 0
          | not (test (bitSize w - 1) w) = w ^>> n
          | otherwise = (w ^>> n) ^| 
			(foldr set 0
                          [i | i <- [bitSize w - (n+1)..bitSize w - 1]])

        w `rol` n
	  | n < 0 || n >= bitSize w = 0
 	  | otherwise =
		    foldr set 0
		      [ (i+n) `mod` bitSize w
		        | i <- [0..bitSize w - 1], test i w ]

        w `ror` n
	  | n < 0 || n >= bitSize w = 0
 	  | otherwise =
		    foldr set 0
		      [ (i-n) `mod` bitSize w
		        | i <- [0..bitSize w - 1], test i w ]

	test n a
	  | n < 0 || n >= bitSize a  = False
	  | otherwise                = (a ^& (1 ^<< n)) /= 0

        set n a
	  | n < 0 || n >= bitSize a  = a
	  | otherwise                = a ^| (1 ^<< n)

        clear n a
	  | n < 0 || n >= bitSize a  = a
	  | otherwise                = a ^& compl (1 ^<< n)

#ifdef __alpha

instance Bits Nat64 where
	(^&) (Nat64 x) (Nat64 y)  = Nat64 (AND x y)
	(^|)  (Nat64 x) (Nat64 y) = Nat64 (OR  x y)
	xor (Nat64 x) (Nat64 y)   = Nat64 (XOR x y)
	compl (Nat64 x)           = Nat64 (COMPL x)
	(^<<) (Nat64 x) y         = Nat64 (LSH x y)
	(^>>) (Nat64 x) y         = Nat64 (RSH x y)
	asr (Nat64 x) y           = Nat64 (RSHA x y)
	bitSize (Nat64 _)	  = 64

#else

instance Bits Nat64 where
	(^&) (Nat64 x) (Nat64 y)  = Nat64 (IAND x y)
	(^|) (Nat64 x) (Nat64 y)  = Nat64 (IOR x y)
	xor (Nat64 x) (Nat64 y)   = Nat64 (IAND (IOR x y) (ICOMPL (IAND x y)))
	compl (Nat64 x)           = Nat64 (ICOMPL(x))
	(^<<) (Nat64 x) y         = Nat64 (x * 2^y)
	(^>>) (Nat64 x) y         = Nat64 (x `div` 2^y)
	asr (Nat64 x) y           = Nat64 (if x >= 0 then x `div` 2^y else - ((-x) `div` 2^y))
	bitSize (Nat64 _)	  = 64
#endif

instance Bits Nat32 where
	(^&) (Nat32 x) (Nat32 y)  = Nat32 (AND x y)
	(^|)  (Nat32 x) (Nat32 y) = Nat32 (OR  x y)
	xor (Nat32 x) (Nat32 y)   = Nat32 (XOR x y)
	compl (Nat32 x)           = Nat32 (MASK32 (COMPL x))
	(^<<) (Nat32 x) y         = Nat32 (MASK32 (LSH x y))
	(^>>) (Nat32 x) y         = Nat32 (RSH x y)
	asr (Nat32 x) y           = Nat32 (MASK32 (RSHA (SEXT32 (x)) y))
	bitSize (Nat32 _)	  = 32


instance Bits Nat16 where
	(^&) (Nat16 x) (Nat16 y)  = Nat16 (AND x y)
	(^|)  (Nat16 x) (Nat16 y) = Nat16 (OR  x y)
	xor (Nat16 x) (Nat16 y)   = Nat16 (XOR x y)
	compl (Nat16 x)           = Nat16 (MASK16 (COMPL x))
	(^<<) (Nat16 x) y         = Nat16 (MASK16 (LSH x y))
	(^>>) (Nat16 x) y         = Nat16 (RSH x y)
	asr (Nat16 x) y           = Nat16 (MASK16 (RSHA (SEXT16(x)) y))
	bitSize (Nat16 _)	   = 16


instance Bits Nat8 where
	(^&) (Nat8 x) (Nat8 y)  = Nat8 (AND x y)
	(^|)  (Nat8 x) (Nat8 y) = Nat8 (OR  x y)
	xor (Nat8 x) (Nat8 y)   = Nat8 (XOR x y)
	compl (Nat8 x)          = Nat8 (MASK8 (COMPL x))
	(^<<) (Nat8 x) y        = Nat8 (MASK8 (LSH x y))
	(^>>) (Nat8 x) y        = Nat8 (RSH x y)
	asr (Nat8 x) y		= Nat8 (MASK8 (RSHA (SEXT8(x)) y))
	bitSize (Nat8 _)	= 8

