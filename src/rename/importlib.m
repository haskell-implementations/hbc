#ifndef QDSTLIB
#define QDSTLIB "/usr/local/lib/lmlc"
#endif
module -- importlib
--
-- Get the library functions, but only when we need them.
-- There are some coding tricks here
--
#include "../misc/flags.t"
#include "../expr/id_t.t"
#include "../expr/constr_t.t"
#include "../expr/ttype_t.t"
#include "../expr/einfo_t.t"
#include "../expr/impexp_t.t"
#include "../expr/read.t"
#include "../expr/types_t.t"
#include "imptuples.t"

export implib, corelib,lmldir;
rec
    LMLENV	= "LMLDIR"		-- Environment variable for LML
and LMLPRELUDE  = "LMLPRELUDE"          -- Environment variable for LML/Haskell Prelude
and LIBDEF	= 			-- Default value of this variable
		  if nuflag then "/usr/local/lib/vlmlc"
		            else QDSTLIB
and LLIB	= "/lib/"		-- place to look for library functions
and PPRE	= "parsed/"		-- prefix for parsed files
and PSUF	= ".p"			-- suffix for parsed files
and INDEX	= "lib.index"		-- the index file

and LPREL	= "prelude.p"

and HLIB	= if H1_3 then "/hlib1.3/" else "/hlib/"		-- place to look for library functions
and MINIPREL	= "PreludeMini.p"
and HCOREPRELS  = if H1_3 then hilibs@["Prelude.p"] else ["PreludeCore.p"; "PreludeRT.p"; "PreludeType.p"]
and HPRELS	= ["Prelude.p"]

and hilibs = if ~ Interactive then [] else 
	case opendir pref in
	   No _ : []
	|| Yes fs : filter (\ f . head 2 (rev f) = "p." & f ~= "Prelude.p") fs
	end

and lmldir = let lmldir' = case getenv LMLENV in
			      Yes s : s
			   || No _  : LIBDEF
			   end 
	     in if Curry then case getenv "HBCDIR" in
		       Yes s : s
		    || No _  : lmldir'
		    end
		else lmldir'

and pref = 
    case getenv LMLPRELUDE in
        No _ : lmldir @ (if Curry then HLIB else LLIB)
    ||  Yes s : s
    end

and open f =
    	case openfile f in
	    No s  : fail (s@": "@f)
	||  Yes s : s
	end
-- Using name to set the name is a trick to avoid reading the file
-- if it isn't needed.  For this to work the files may only contain function
-- definitions.
and name f i =
	let (mkimpid _ t i _) = i in
	mkimpid (mkids f) t i None
and getfun f =
	let (r, i) = readimpid (open (pref@PPRE@f@PSUF)) in
--	name f i 	-- make sure we close the file  -- fds are gc:ed now (if r=r then i else fail "Huh?")
        name f (if r=r then i else fail "Huh?")

and lmllibfuncs = map getfun (choplist (splitat '\n') (open (pref@INDEX)))

and getimport s =
    case (readinterface s) in
	(_, i) : i
    end

-- LML prelude
and lmlops = [ -- prec is wrong -- A hack
 mkfixid (map mkids ["_~="; "_>="; "_<="; "_??"; "_+."; "_-."; "_*."; "_/."; "_+#"; "_-#"; "_*#"; "_/#"; "_%#"; "_."; "_="; "_>"; "_<"; "_+"; "_-"; "_*"; "_/"; "_%"; "_~"; "_&"; "_|"; "_@"; "_?"; "_^"]) (Infix 9)]
and lcorelib = 
    case getimport (open (pref@LPREL)) in
    (mkimport a b c d e f g qual asname) : [mkimport a b (lmlops@c) d e f g qual asname; imptuples]
    end
and limplib = [mkimport (mkids "_PreludeLML") [] [] lmllibfuncs false [] [] false None]

-- Haskell prelude
and minimplib = [getimport (open (pref@MINIPREL))]
and nulllib  = mkimport (mkids "_Prelude") [] [] [] false [] [] false None
and hcorelib = if NoPrelude then minimplib else imptuples.map (\x.getimport (open (pref@x))) HCOREPRELS
and himplib  = if NoPrelude then [nulllib] else if H1_3 then hcorelib else map (\x.getimport (open (pref@x))) HPRELS

--  corelib contains types etc.
and corelib = if Curry then hcorelib else lcorelib
--  implib contains functions etc.
and implib = if Curry then himplib else limplib
end
