module
export Teinfo, Finfo, Arginfo, Argpos;
rec type Teinfo =
	strict +		-- Evaluate the expression
	noeval +		-- Don't EVAL the variable
	restr [Int] Ttype +	-- Do a type restriction on the expression, some tyvars are no generic
	inline +		-- Expand the binding inline
	spark (List Id) + 	-- SPARK the variables
	doeval (List Id) +	-- EVAL the variables
	chkind +		-- Check for possible indirection, but don't EVAL
	trestr Ttype +		-- type restriction for information
	srestr Ttype +		-- another type restriction for information
	noarrow Ttype +		-- variable type has no arrow
	notchk +		-- don't typecheck inside this expression
	forcearity Int +	-- force function to have this arity
	overload +		-- allow top level non-lambda overloading
--	metsel Int Int +	-- application is a method selection with the first n args sent to the vector, method has arity m
--	vecsel +		-- application is a superclass vector selection
	metcall	+		-- application is a method call in the vector
	vecreg2 +		-- 
	vectordef +		-- body is a vector definition
	limitok +		-- no limit check necessary for case
	unboxedvar +		-- this variable is unboxed
	unboxedarg +		-- compute and send this argument unboxed
	unboxedexpr +		-- compute this expression unboxed
	unboxvars + 		-- unbox the following doeval variables
	specialcall (List(Arginfo#Argpos)) + -- use special entry point for following call
	notconst +		-- not to be regarded as constant (i.e. has side effect!)  NOT IMPLEMENTED IN Gcode/mcode yet!!
        position String Int +   -- psoition in file
        stmtpat +		-- from statement expansion
        stmtfailpoint Texpr +   -- insert statement zero
        dropctx Ttype (List Int)-- internal to checking universals
and type Finfo = finfo Int (List (List (Arginfo#Argpos))) (BT#BT) Int (Option Texpr)
		 -- arity, entries,                       strict, frsize, inline
and type Arginfo = AInothing + AIeval + AIunboxed
and type Argpos = APframe + APregister
end
