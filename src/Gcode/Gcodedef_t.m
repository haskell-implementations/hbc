module
export Gbasicop, Gbasicconstr, Glabel, Galloc, Gcode, Gvalue, Gcodef, Gcodes, Gstubs, HPinfo;
rec type Gbasicop =
	ADD + SUB + MUL + DIV + MOD + NEG +
	EQ + NE + LT + GT + LE + GE +
	CHR + ORD + TAG +
        AND + OR + XOR + COMPL + LSH + RSH + RSHA +
	FTOI + ITOF + DFTOSF + SFTODF +
        INDEX + SQR

and type Gbasicconstr = Gbint + Gbchar + Gbtag + Gbdfloat + Gbinteger + Gbsfloat + Gbstring + Gbother
and type Gvalue = GvInt Int + GvDFloat Double + GvSFloat Double + GvChar Char
and type Glabel = Label Int + Notalabel + Continue
and type Galloc = Aheap + Astack
and type HPinfo == String # String
and type Gcodef == (Id#Int) # (List Gcode) # (Option (List Gcode))
and type Gcodes == List Gcodef
and type Gstubs == List Int
and type Gcode =
	PUSH Int		+ -- offset from top of stack
	PUSHGLOBAL Id		+
	EVAL Gstubs		+
	LABEL Glabel 		+
	JMP Glabel		+
	JFALSE Glabel		+
	JFUN Int		+ -- tailcall with n arguments
	CALLFUN Int Gstubs	+ -- call with n arguments
	UNWIND			+ -- return via top node

	ALLOC Int		+ -- allocate n hole nodes, and push n ptrs
	BCONSTR Gbasicconstr	+ -- mkint, mkbool, mkchar etc
	MKAP String		+ -- construct application
	MKCAP String		+ -- construct canonical application
	MKAPLV Id Int		+ -- Make application vector
	UPDATE Gbasicconstr Int	+
	UPDATEINDIR Int		+
	MOVE Int		+
	RECBLOCK (List (List Gcode))	+ -- corresponds to ALLOC n; gs; and UPDATEs
	CONSTBLOCK (List Gcode)	+ -- corresponds to gs, but is constant
        UPDTRACE (List Int)	+ -- update trace for gen gc

	CONSTR HPinfo ConstructorType Int Int	+
	CASE Int (List(Int#Int#Glabel)) Glabel +
	SPLIT (List Bool) Int Int + -- Push arguments of a constructor

	BASICOP Gbasicconstr Gbasicconstr Gbasicop	+ -- ADD, SUB etc
	BIGOP Gbasicconstr Gbasicop		+ -- EQ, NE, ...
	PUSHBASIC Gvalue	+ -- push basic value onto basic value stack
	PUSHV Int		+ -- offset from top of stack
	POP Int			+ -- pop n entries from the pointer stack
	POPV Int		+ -- pop n entries from the value stack
	GET Gbasicconstr	+ -- Get int, etc
	GETTAG			+ -- Get constructor tag
	GETMETHOD Int		+ -- Get entry in method vector

--	FUNSTART Id Int		+ -- Function, and no of arguments
        REST Gcodef		+ -- just used in mcode.m to handle the peculiar structure of M
	FUNEND			+ --   - " -
	SFUNSTART Id Int	+ --   - " -
	-- optimizations
	RET			+ -- Real return, skip unwind state
	JGLOBAL Int Id		+ -- Direct jump to a function
	CALLGLOBAL Int Id Gstubs+ -- Direct call to a function
	BUPDRET Gbasicconstr Int+ -- Return a basic value
	CUPDRET ConstructorType Int Int Int+ -- Return a constructor
	UPDRET Int		+	-- update & return

	JMETHOD Int Int		+ -- Method jump: arity and method number
	CALLMETHOD Int Int Gstubs	+ -- Method call: arity and method number

	TEVAL 			+ -- Tests if a value is evaluated
	SCALLFUN Int 		+ -- Calls stingy code with n arg. at SFUNSTART
	SCALLGLOBAL Int Id      + -- Direct stingy call to a function
	JTRUE Glabel  		+ 

	-- The CONSTR TYPE instruction is translated into 0, 1 or 2
	-- of the following variants of constr.
	CNIL HPinfo Int		+	-- Nil constructor, nullary
	CPAIR HPinfo Int	+	-- Pair constructor, binary
	CTAG HPinfo Int		+	-- Tag constructor, unary
	CVEK HPinfo Int		+	-- Vector constructor
	CBASIC Gvalue		+	-- INT, BOOL, CHAR etc, constant
	CSTRING (List Char) 	+	-- string constant
--	CMVECTOR Id (List Id)	+	-- method vector

	-- The SPLIT instruction is translated into 0, 1 or 2
	-- of the following variants of split.	
	SPLITPAIR Bool Bool	+
	SPLITTAG		+
	SPLITVEK (List Bool) Int	+

	-- Memory alloc mode
	AMODE Galloc		+
	ANNOT String		+	-- misc annotations

        PUSHCFUNCTION Bool String       -- push pointer to C function
end
