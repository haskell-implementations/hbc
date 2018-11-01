/************************************************************************
*									*
*		Translation to FLIC data primitives			*
*									*
*	This pass removes occurrences of Econstr, Ecase and Efailmatch  *
*	by translating them into the FLIC primitives CASE-..., 		*
*	PACK-..., and UNPACK-...					* 
*									*
*	It should be done BEFORE lambda-lifting, 			*
*	but AFTER type-checking						* 
*									* 
************************************************************************/

module

#include "../ExprE/Expr_t.t"
#include "../ExprE/Eutil.t"		/* For Emkint */
#include "../expr/constr_t.t"
#include "../expr/constrfun.t"
#include "../expr/tinfo.t"
#include "../expr/id_t.t"
#include "../expr/id.t"

export fconstr;

rec

-- fconstr:  e:Expr	-- The expression to transform
--	  -> u:Int	-- Unique number used for generating new variable names
--	  -> (Expr,	-- The transformed expression
--	      Int)	-- The resulting unique number 
fconstr e u = fc (Efailmatch 0) e u
		-- The default expression argument to fc should never be used

and
   -- fc: Expr 	  -- The default expression to substitute for Efailmatch
   --  -> Int	  -- The name supply
   --  -> Expr    -- The expression to transform
   --  -> (Expr,  -- The transformed expression
   --  ->  Int)	  -- The returned name supply

   -- The first cases are completely straightforward
   fc d (Elet r defs body) u
   = let    (new_defs, u1) = fmap (fc_def d) u defs 
     in let (new_body, u2) = fc d body u1
     in (Elet r new_defs new_body, u2)

|| fc d (Eidapl id args) u
   = let (new_args, u1) = fmap (fc d) u args
     in (Eidapl id new_args, u1)

|| fc d (Einfo info expr) u
   = let (new_expr, u1) = fc d expr u
     in (Einfo info new_expr, u1)

|| fc d (Emodule i exp defs) u
   = let (u1, new_defs) = mapstate (\uu.\dd.let (ndd,nuu)=fmap (fc_def d) uu dd in (nuu, ndd)) u defs
     in (Emodule i exp new_defs, u1)

|| fc d (Elaml vs body) u
   = let    (new_body, u1) = fc d body u
     in (Elaml vs new_body, u1)

   -- Now the interesting cases
|| fc d (Econstr constr args) u
   = let (new_args, u1) = fmap (fc d) u args
     in (ccnv constr new_args, u1)
	-- Convert to an application of PACK...

|| fc d (Ecase discrim cases default) u 
   = let    (new_discrim, u1) = fc d discrim u
     in let (new_default, u2) = fc d default u1
     in fcase new_discrim cases new_default u2
	-- Convert to an application of CASE... and UNPACK...

|| fc d (Efailmatch n) u = (d, u)

and fc_def d (id, rhs) u 
    = let (new_rhs, u1) = fc d rhs u
      in ((id, new_rhs), u1)


/************************************************************************
*									*
*			Case translation				*
*									*
************************************************************************/

and
    fcase discrim cases default u
    -- discrim and default have already been transformed but cases havn't
    =  	let rec
	    (default_var, u1) = new_var "FAIL_VAR" u
	and (discrim_var, u2) = new_var "DISCRIM_VAR" u1
	and trivial_default = is_trivial default
	and trivial_discrim = is_trivial discrim
	and default_expr = if trivial_default then default 
			   else Eidapl default_var []
	and discrim_expr = if trivial_discrim then discrim 
			   else Eidapl discrim_var []
	and
	    (all_cases, u3) = fill_cases discrim_expr cases default_expr u2
	and
	    new_case = if (singleton all_cases) then 
			  hd all_cases
		       else
			  Eidapl (make_flic_prim_id 
					("CASER-" @ itos (length all_cases))) 
				 (discrim_expr . all_cases) 
			-- Note CASER not CASE, with discriminated expression
			-- first; for convenience in GRIP 4-stroke impl of 
			-- case analysis
	and
	    new_case1 = if trivial_discrim then new_case
			else Elet false [(discrim_var, discrim)] new_case
	and
	    new_case2 = if trivial_default then new_case1
			else Elet false [(default_var, default)] new_case1
	in
	(new_case2, u3)

and
    fill_cases discrim cases default u
    -- Returns a (List Expr), which covers all cases densely
    -- If there are too many cases, or they are too sparsely scattered,
    -- fill_cases returns a singleton list in which
    -- the expression does the case-analysis by conditionals
    = let rec
	    ((first_constructor,_,_)._) = cases	-- Assumes >= 1 case in list
	and type_info = ctinfo first_constructor
	and no_of_constructors = get_no_of_constr_from_tinfo type_info

	and (new_cases, u1) = fmap transform_case u cases
	    -- new_cases is same as cases, except RHSs are transformed
	and transform_case (constr, vars, expr) u
	    = let (new_expr, u1) = fc default expr u
			-- Notice that the branches of the case are transformed
			-- with the *new* default expression
	      in ((constr, vars, new_expr), u1)
		
	-- fill is used to get a dense list of cases; it is used
	-- for non-numeric cases
	-- Assumption: case-list in Ecase is in ascending order of tag
	and fill tag [] = rept (no_of_constructors - tag) default
	||  fill tag (cases as ((constr, vars ,expr).rest_cases))
	    = if (tag = (cno constr)) then
			(unpack constr vars expr discrim) . 
			fill (tag+1) rest_cases
	      else
			default . fill (tag+1) cases

	-- numeric_conditional is used to generate a conditional expression
	-- for numeric case-expressions
	and numeric_conditional [] = default
	||  numeric_conditional ((constr, vars, expr) . rest_cases)
	    = Eidapl if_id [(Eidapl inteq_id [	discrim; 
						Emkint (cno constr)
					     ]);
			    expr;
			    numeric_conditional rest_cases ]
     in
     case (constrtype first_constructor) in
        Gint : ([numeric_conditional new_cases], u1)
     || _ :    (fill 0 new_cases, u1)
     end

and 
    unpack constr []   expr object = expr
||  unpack constr vars expr object
    = let
	unpack_help_function = Elaml vars expr
      in
      Eidapl (make_flic_prim_id ("UNPACKR!-" @ (itos (carity constr)))) 
	     [object; unpack_help_function]
      -- Note the order of arguments: object to be unpacked first.
      -- This is for convenience for the GRIP 
      -- 4-stroke implementation of UNPACKR!


/************************************************************************
*									*
*		PACK translation					*
*									*
************************************************************************/

and
    -- Convert a constructor application to use FLIC primitives
    -- The args are already dealt with
    ccnv (c as Cconstr _ _ _ d _ l) args =
    	case constrtype c in
	   Gtype :	-- A normal constructor, so use an ordinary application
			-- of the PACK primitive to the args
			Eidapl (make_flic_prim_id 
					("PACK-"@itos (length l)@"-"@itos d)) 
			       args

	|| _ :		-- Gint, Gchar, Gstring s; args should be []!
			Econstr c args	
	end


/************************************************************************
*									*
*			Utilities					*
*									*
************************************************************************/

and
    make_flic_prim_id s = mkid 0 s (idi_var var_unknown Onotype None) noorigname
			  -- mkid is a constructor for type Id
		-- I HOPE that 0 is right for the first argument, but
		-- I havn't much idea!

and if_id = make_flic_prim_id "IF"
and inteq_id = make_flic_prim_id "INT="

and new_var name u = (mkid u name (idi_var var_unknown Onotype None) noorigname, u+1)

and 
    -- is_trivial is used to decide whether to duplicate an expression
    -- to to create a binding for it.
    is_trivial (Eidapl _ []) 	= true
||  is_trivial (Einfo _ e) 	= is_trivial e
||  is_trivial (Efailmatch _)	= true
||  is_trivial _		= false

and 
    -- fmap maps a function down a list, plumbing the name supply
    fmap f u = let ff x (fxs, u) = let (fx, u1) = f x u 
				   in (fx.fxs, u1)
	       in
	       reduce ff ([],u) 

and singleton [x] = true
||  singleton xs = false
end
