/*
**	Numset:		Data type Set of (non-negative) numbers
**		
**			Operations:
**
**	NSlisttoset l:	Make a numset from a list of integers. Negative
**			integers are ignored.
**	NSempty:	The empty set.
**	NSsettolist s:	Make a list of integers from a numset, in 
**			ascending order.
**	NSsettoblist s:	Make a list of booleans from a numset, one
**			boolean for each number in 0,1,2, ... .
**	NSunion a b:	The union of a and b.
**	NSintsect a b:	The intersection between a and b.
**	NSsub a b:	Set 'subtraction', a-b.
**	NSadd n s:	Add the number n to the numset s.
**	NSmem n s:	Returns true if n is a member of s.
**	NSissubset a b:	Returns true if a is a subset of b (or equal).
**
**	NSupperlim s:	An upperlimit to the biggest number in s.
**
*/
module
import ___NSlisttoset:	List(Int) -> Int -> Numset {# ARITY ___NSlisttoset = 2 #}{# STRICTNESS ___NSlisttoset = "F,F" ST #};
import ___NSunion:	Numset -> Numset -> Numset {# ARITY ___NSunion = 2 #}{# STRICTNESS ___NSunion = "F,F" ST #};
import ___NSintsect:	Numset -> Numset -> Numset {# ARITY ___NSintsect = 2 #}{# STRICTNESS ___NSintsect = "F,F" ST #};
import ___NSsub:	Numset -> Numset -> Numset {# ARITY ___NSsub = 2 #}{# STRICTNESS ___NSsub = "F,F" ST #};
import ___NSadd:	Int    -> Numset -> Numset {# ARITY ___NSadd = 2 #}{# STRICTNESS ___NSadd = "F,F" ST #};
import ___NSisempty:	Numset -> Bool {# ARITY ___NSisempty = 1 #}{# STRICTNESS ___NSisempty = "F,F" ST #};
import ___NSmem:	Int    -> Numset -> Bool {# ARITY ___NSmem = 2 #}{# STRICTNESS ___NSmem = "F,F" ST #};
import ___NSsub:	Numset -> Numset -> Numset {# ARITY ___NSsub = 2 #}{# STRICTNESS ___NSsub = "F,F" ST #};
import ___NSissubset:	Numset -> Numset -> Bool {# ARITY ___NSissubset = 2 #}{# STRICTNESS ___NSissubset = "F,F" ST #};
import ___NSupperlim:	Numset -> Int {# ARITY ___NSupperlim = 1 #}{# STRICTNESS ___NSupperlim = "F,F" ST #};

export 	NSempty, NSisempty, NSlisttoset, NSsettolist, NSsettoblist, 
	NSunion, NSintsect, NSsub, NSadd, NSmem, NSissubset, NSupperlim;


	NSempty		= ___NSlisttoset [] 0

and	NSisempty s	= ___NSisempty s

and	NSlisttoset l 	= let rec max m  []           = m
	     	               || max m (x.l) & (x>m) = max x l
	     	     	       || max m (_.l)         = max m l
			  in ___NSlisttoset l (max 0 l)

and	NSsettolist s 	= let rec f i u s & (i > u)        = []
			       || f i u s & (___NSmem i s) = i. f (i+1) u s
			       || f i u s                  = f (i+1) u s
			   in f 0 (___NSupperlim s) s

and	NSsettoblist s 	= let rec f i u s & (i > u)        = []
			       || f i u s & (___NSmem i s) = true . f (i+1) u s
			       || f i u s                  = false. f (i+1) u s
			   in f 0 (___NSupperlim s) s

and	NSunion a b 	= ___NSunion a b
and	NSintsect a b 	= ___NSintsect a b
and	NSsub a b	= ___NSsub a b
and	NSadd n s	= ___NSadd n s
and	NSmem n s	= ___NSmem n s
and	NSissubset a b	= ___NSissubset a b
and	NSupperlim s	= ___NSupperlim s
end



