module -- cunrec
--
--  unrec module for conctype version of LML
--
#include "../expr/types_t.t"
#include "../expr/id.t"
#include "../expr/exprid.t"
#include "../misc/misc.t"
#include "../misc/util.t"

export cunrec;


-- should we import eandify, sccds, etc from the other modules ?
--
-- 1.	flatten out the bindings -- local bindings removed!!
--	([mkbmulti p1 exp1, mkbpat p2 exp2,...], [(p1,exp1),(p2,exp2),...])
--	flatbnd : Binding -> 
--		List(Binding) * (List(Texpr(pattern) * Texpr(expr))
-- 1b.  remove the binding constructors
-- 2a.	get the identifiers used in each binding (pattern and expression)
--	[([f1,f2,...],[g1,g2,g3,...]),([x1,x2,...],[h1,h2,...]),...]
--	idexpr, idexpr ?
-- 2b.  spread the letbound identifiers.
--      [(f1,[g1,g2,g3,...]),(f2,[g1,g2,g3,...]),...]
-- 2.   build strongly connected components
--	[[(f1,[g1,g2,g3,...]),(x1,[h1,h2,h3,...]),...],[...],...]
--      scceq eqid  : List(Id*List(Id)) -> List(List(Id * List(Id)))
-- 4.   combine the components to be consistent with the original patterns
--	[[(p1,exp1),(p2,exp2)],[(p3,exp3)],...]
--	List(List(Texpr * Texpr))
-- 5.	try to reconstruct everything which is not so easy.
--	Make everything mutually recursive!!


rec -- List(Binding) -> Binding
    eandify   [b]       = b
||  eandify   (b.bs)    = mkband b (eandify bs)
||  eandify   []        = mkbnull

and -- isrecursive : Texpr # Texpr -> Bool
    isrecursive (p,e) =
	not (null (intereq eqid (mkseteq eqid (idexpr p))
	                        (mkseteq eqid (idexpr e))))
and -- mapfs : ((*a->*b)->((List (*a # *a))->(List (*b # *b))))
    mapfs f = map (\(p,e).(f p, f e))

and -- rmbind : Binding -> Texpr # Texpr
    rmbind (mkbpat [(p,e)]) = (p,e)
||  rmbind (mkbmulti p e)   = (p,e)

and -- emlet : List(Binding) -> Texpr -> Texpr
    emlet []  e = e
||  emlet [b] e = mkletv ((if (isrecursive (rmbind b)) then mkbrec else I) b) e
||  emlet bs  e = mkletv (mkbrec (eandify bs)) e

and 

    I x = x

and -- mmod : List(Binding) -> Binding
    mmod []  = mkbnull
||  mmod [b] = (if (isrecursive (rmbind b)) then mkbrec else I) b
||  mmod bs  = mkbrec (eandify bs)

and -- prettyprintfunctions
    show_idlist l = show_list prid l

    
and -- sccfun : List(Binding) -> List (List (Binding))
    -- strongly connected components of bindings, 
    -- first make a directed graph, (ness)
    -- represented as a list [([f1,[f1,f2,f3]),(f2,[...]),...]
    -- then compute the strongly connected components
    -- and finally create a list suitable for reconstructing the
    -- let expression with mutually recursive components.
    -- sccfun : List(Binding) -> List(List(Binding))
    sccfun ds =
	-- dsids a database of the bound identifiers and bindings
	-- [([x1,x2,x3], mkbmulti patt expr),...]
	let dsids = map (\b.let (p,e) = rmbind b in (idexpr p, b)) ds in
 	-- ns = all defined identifiers in the let-expression
	let ns = concmap fst dsids in
	-- make dependency graph
	let spreadids(p,e) =
		let ids = intereq eqid (mkseteq eqid (idexpr e)) ns in
		map (\i.(i,ids)) (idexpr p)
	in
	let x1 = concmap (spreadids o rmbind) ds in
	let x2 = scceq eqid x1 in
	let sccs = map (map fst) x2
	in  /*
		  trace ("x1: " @ 	show_int(length x1) 		@ "\n")
	   	( trace ("dsids: " @	show_int(length dsids) 		@ "\n")
	    */
	    (combinescc sccs dsids)
	    /* ) */
	     
and -- setminusid : List(Id) -> List(Id) -> List(Id)
   setminusid s t = filter (\x.(not (member eqid x t))) s

and -- combinescc :  List (List Id) -> List (List(Id) # Binding)) -> List (List Binding)
    combinescc scs bndDB
    = let rec -- get all bindings for the identifiers in one strongly
	      -- connected component and remove the corresponding
	      -- bindings from the database
	      -- getcomp : (sc:List(Id)) -> 
	      --           (completebndDB:List(List(Id)*Binding)) ->
	      --	   (bndDB:List(List(Id)*Binding))
	      -- 		-> (List(Binding)) *
	      --		   (bndDB:List(List(Id)*Binding))
          getbinds sc bndDB []  = ([],bndDB)
      ||  getbinds [] bndDB bDB = ([],bndDB)
      ||  getbinds sc bndDB (bDB as ((pids,b).bDBR)) = 
		let bids = intereq eqid sc pids
		in  if null bids
		    -- this pattern does not contain any id in the sc
		    -- just go on
		    then getbinds sc bndDB bDBR
		    -- this pattern contain identifiers from the sc
		    else let (bs,bndDBR) = getbinds (setminusid sc bids) (rmDB pids bndDB) bDBR
			 in  (b.bs,bndDBR)
      and
	  rmDB []  bDB = bDB
      ||  rmDB rm  []  = []
      ||  rmDB rm ((pids,b).bDBR) = 
		let scR = setminusid rm pids
		in  if null scR
		    then bDBR			-- can only occure once
		    else (pids,b).rmDB rm bDBR
      in  case scs
	  in  []		: []
	  ||  (sc.scs)		: let (bs,bDB) = getbinds sc bndDB bndDB
				  in  bs.combinescc scs bDB
	  end


and -- flatbnd : Binding -> List (Binding)
    --    flatten a binding to a list consisting of the primitive 
    --    bindings (mkbpat and mkbmulti)
    flatbnd (mkbrec d)        = flatbnd d
||  flatbnd (mkband d1 d2)    = flatbnd d1 @ flatbnd d2
||  flatbnd (mkbpat [(p,e)])  = [mkbpat[(p,cunrec e)]]
||  flatbnd (mkblocal d1 d2)  = flatbnd d1 @ flatbnd d2
||  flatbnd (mkbmulti p e)    = [mkbmulti p (cunrec e)]
||  flatbnd (mkbnull)         = []
||  flatbnd (mkbtype _ _ _ _) = []
||  flatbnd (mkberror _)      = []
||  flatbnd (mkbpragma _)     = []

and
    -- cunrec : Texpr -> Texpr
    cunrec (mkap f a) = mkap (cunrec f) (cunrec a)
||  cunrec (mklam i e) = mklam i (cunrec e)
||  cunrec (mkcase e cl) = mkcase (cunrec e) (mapfs cunrec cl)
||  cunrec (mkletv b e) = itlist emlet (sccfun (flatbnd b)) (cunrec e)

||  cunrec (e as mkident _) = e
||  cunrec (mkmodule i fixl impl expl b) = 
	mkmodule i fixl impl expl 
		(mkbrec (eandify (map mmod (sccfun (flatbnd b)))))
||  cunrec (e as mkconst _) = e
||  cunrec (e as mkcfunction _ _) = e
||  cunrec (e as mkerror _) = e
||  cunrec (mkas i e) = mkas i (cunrec  e)
||  cunrec (mkcondp p c) = mkcondp (cunrec  p) (cunrec  c)
--  lazyp cannot occur
||  cunrec (mkinfo t e) = mkinfo t (cunrec  e)
||  cunrec (mkconstr c es) = mkconstr c (map cunrec es)
||  cunrec (e as mkfailmatch _) = e
||  cunrec (e as mkbrack _ _) = e -- PROVISORISKT!!!!!!!!

end

