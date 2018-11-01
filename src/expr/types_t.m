#define String (List Char)
module
export Texpr, Binding, IdOrConstr, Asstype, Prod, Cgs, Lex, Atype, Const, Qual, Fixid, Import, Stmt;
rec type Texpr =
	  mkap Texpr Texpr		
	+ mklam Texpr Texpr
	+ mkcase Texpr (List (Texpr#Texpr))
	+ mkletv Binding Texpr
	+ mkident Id
	+ mkmodule Id (List Fixid) (List Impid) (Option (List Expid)) Binding
	+ mkconst Const
	+ mkbrack (List (Ttype # (List (IdOrConstr # Prod)))) (List Lex)
	     -- conctypes (grammar lexical_symbols)
	     -- The grammar is filled in in the rename pass
	+ mkerror String
	+ mkas Id Texpr	-- Only in patterns
	+ mkcondp Texpr Texpr	-- Only in patterns
	+ mklazyp Texpr -- Only in patterns
	-- the following variants are not part of the abstract syntax, but
	-- are added later by transformations
	+ mkconstr Constr (List Texpr)
	+ mkfailmatch Int
	+ mkinfo Teinfo Texpr
	+ mklistf Int (List Texpr)
	+ mklistg Texpr (List Qual)
	+ mkhmodule Id (Option (List Expid)) (List Import) (List Fixid) Binding
	+ mkwhere (List (Texpr#Texpr)) Binding
        + mkcfunction Bool Id
        + mkdo Stmt
        + mkrecord Texpr (List (Id#Texpr)) [Atype]

and type Fixid = mkfixid (List Id) Fixity
and type Import = mkimport Id (List Impid) (List Fixid) (List Impid) Bool (List Expid) (List (Id#Id)) Bool (Option Id)

and type Stmt =
          mksexp Texpr
        + mksexpstmt Texpr Stmt
        + mkslet Binding Stmt
        + mksbind Texpr Texpr Stmt

and type IdOrConstr =                      --  for conctypes
	  ccon Constr 
	+ cid Id

and type Binding =
	  mkbtype Ttype (List Atype) (Option (List Id)) Bool	-- type, constructor, derived insts, isomorphic flag
	+ mkbctype Ttype (List Prod)                            -- conctype 
	+ mkbpat (List (Texpr#Texpr))
	+ mkbmulti Texpr Texpr
	+ mkband Binding Binding
	+ mkbrec Binding
	+ mkberror String
	+ mkblocal Binding Binding
	+ mkbnull		-- nothing bound
	+ mkbsyn Ttype Ttype
	+ mkbclass CDecl Binding
	+ mkbinstance IDecl Binding (Option Id)			-- type, bindings, and id for vector (after rename)
	+ mkbdefault (List Ttype)
	+ mkbsign (List Id) Ttype
        + mkbpragma Pragma
	+ mkbview Ttype Ttype (List Atype) Binding

and type Asstype = 
        arightassoc + aleftassoc + anonassoc + abothassoc
and type
    Prod = 
         mknormal (List Cgs) (Int # Asstype)     -- [| ...... |] 
       + mkforget (List Cgs) (Int # Asstype)     -- [|| ...... ||] 

     /* used in grammars */
and type
    Cgs =
        mkcnt Ttype                     -- nonterminal
      + mkct Char                       -- terminal
      + mkctint Int                     -- int terminal
      + mkctid String                   -- id terminal
      + mkctsym String                  -- sym terminal
      + mklist1 Ttype (List Cgs) Int    -- {<type> terminals  ...}+ counter
      + mklist0 Ttype (List Cgs)        -- {<type> terminals  ...}* 
      + mklistend Int Bool              -- only used internal in earley to
        				-- handle parsing of lists 

    /* used in patterns and expressions */
and type
    Lex =
        mklt Char                           -- terminal
      + mkltint Int                         -- int terminal
      + mkltid String                       -- id terminal
      + mkltsym String                      -- sym terminal 
      + mkunq Texpr                         -- unquote  

and type Atype =
	  mkcons Id (Bool # (List Int) # (List Assert)) (List (Ttype#Bool#(Option Id))) Bool
--               name (univ/exist local-var exist-context)   type&strictness&selector  
--			last boolean indicates that the definition was with selector syntax
and type Qual =
          mkqgen Texpr Texpr
	+ mkqfilter Texpr
        + mkqlet Binding

and type Const =
	  cint Int
	+ cchar Char
	+ cstring String
	+ cfloat String
	+ cinteger String
	+ crational String
end
