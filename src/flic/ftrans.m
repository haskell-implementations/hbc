module
#include "Flic_t.t"
#include "fprint.t"
#include "../expr/id_t.t"
#include "../expr/types_t.t"
#include "../expr/ttype_t.t"
#include "../expr/einfo_t.t"
#include "../expr/id.t"
#include "../expr/impexp_t.t"
#include "../expr/constr_t.t"
#include "../transform/misc.t"
#include <Option>
export ftrans;
rec
    mi n = mkident (mkids n)
and mu n = mi ('_'.n)
and x = mi "x"
and y = mi "y"
and I = mklam x x
and dol = mi "_"
and tiu = mktinfo UT 0 false false [] false false None
and UT = mktcons (mkid 0 "?!" (idi_type mkkground UT 0 tiu [] None) noorigname) []	-- Unknown Type
and bot = fail "bot"
and mkc c d n m f = mkident 
	(mkid (n*10000+d)		-- id #
	      ('_'.c)			-- id name
	      (idi_constr		-- id info:
		  UT			-- constructor type (dummy)
	          (false,[],[])
		  (rept n (UT, false, None))	-- subpart type(dummy),strictness
		  (d-1)			-- constructor number
		  (rept m (mkcons bot (false,[],[]) (if f then [] else [bot]) false))
		  false
		  None)
	      noorigname)	-- other constructors
and mktuple l = revitlist (\a.\f.mkap f a) l (mkident (mkpids ("P#"@itos (length l))))
and mkaps f sa = revitlist (\a.\f.mkap f a) sa f
and cmp s = mklam x (mklam y (mkcase (mkap (mkap (mi s) x) y)
	[(mkident (mkids "_false"), mkc "PACK-1-0" 1 0 2 true);
	 (mkident (mkids "_true"),  mkc "PACK-2-0" 2 0 2 true)]))
and trtab = [
("_INT",	mi "Pneg");
("+INT",	mi "Padd");
("-INT",	mi "Psub");
("*INT",	mi "Pmul");
("/INT",	mi "Pdiv");
("%INT",	mi "Pmod");
("<INT",	cmp "Plt");
("<=INT",	cmp "Ple");
("=INT",	cmp "Peq");
(">=INT",	cmp "Pge");
(">INT",	cmp "Pgt");
("!=INT",	cmp "Pne");
("INT-TO-ASCII",mi "_chr");
("ASCII-TO-INT",mi "_ord");
("EXPLODE",	I);
("IMPLODE",	I);
("SEQ",		mi "Pseq");
("ABORT",	mkap (mi "Pfail") (mkconst (cstring "ABORT")))]
and nt' ('T'.'U'.'P'.'L'.'E'.'-'.n) = mi ("F#"@n)
||  nt' ('S'.'E'.'L'.'E'.'C'.'T'.'-'.r) =
	let (sn, si) = splitat '-' r in
	let n = stoi sn
	and i = stoi si in
	mklam (mktuple (rept (i-1) dol @ [x] @ rept (n-i) dol)) x
||  nt' (c as 'P'.'A'.'C'.'K'.'-'.r) =
	let (sd, sn) = splitat '-' r in
	let d = stoi sd
	and n = stoi sn in
	mkc c (stoi sd) (stoi sn) 0 false
||  nt' n = mu n
and nt n = assocdef n trtab (nt' n)
and fta (Fap f a) es	= fta f (a.es)
||  fta (Fname ('T'.'U'.'P'.'L'.'E'.'-'.n)) es & (length es = stoi n) =
	mktuple (map ft es)
||  fta (Fname ('C'.'A'.'S'.'E'.'-'.sn)) es =
	let cn = stoi sn in
	if length es ~= cn+1 then fail "CASE arg" else
	let d = last es in
	let rec flat = all (\(p, _).head 2 (reverse (let (mkident (mkid _ s _ _)) = leftmost p in s)) = "0-") pes
	and pes = map2 f (head cn es) (from 1)
	    where f (Fap (Fap (Fname ('U'.'N'.'P'.'A'.'C'.'K'.'-'.r)) e) d') _
		    & (d=d') =
			let (sd, sn) = splitat '-' r in
			let n = stoi sn in
			let vars = for 1 n (mu o itos) in
			(mkaps (mkc ("PACK-"@r) (stoi sd) n cn flat) vars,
			 mkaps (ft e) vars)
	       || f e m = (mkc ("PACK-"@itos m@"-0") m 0 cn flat, ft e)
	in
	mkcase (ft d) pes
||  fta f es		= mkaps (ft f) (map ft es)
and
    ft (Fname n)	= nt n
||  ft (Fnumber i)	= mkconst (cint i)
||  ft (Fchar c)	= mkconst (cchar c)
||  ft (Fstring s)	= mkconst (cstring s)
||  ft (Fap f a)	= fta f [a]
||  ft (Flam n f)	= mklam (mu n) (ft f)
||  ft (Flet is_rec ns fs b) 
    = let binding = mkbpat (combine (map mu ns, map ft fs))
      in let rec_binding = if is_rec then mkbrec binding else binding
      in mkletv rec_binding (ft b)

||  ft (Fannot (Annot0 "inline") f)	= mkinfo inline (ft f)
||  ft (Fannot a f)	= ft f
||  ft f		= fail ("No match in ft: "@fprint f@"\n")
and ftrans f = 
	let e = ft f in
	mkmodule (mkids "_") [] []
		(Some [mkexpid (mkids "Pmain")])
		(mkbpat [(mi "Pmain", mklam dol e)])
end
