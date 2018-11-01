module
#include "../misc/util.t"
#include "../misc/oset.t"
#include "../mcode/mcodedef_t.t"
export extern_declarations, fixlabel, globvar;

rec get_id b (glob i) = [(i, textref i, b)]
 || get_id b (idlit i) = [(i, textref i, b)]
 || get_id b (retaddr i) = [(i, true, b)]
 || get_id b _ = []
and get_id_used (Mmove a1 a2)  = get_id true a1 @ get_id true a2
 || get_id_used (Mmovesf a1 a2)  = get_id true a1 @ get_id true a2
 || get_id_used (Mmovedf a1 a2)  = get_id true a1 @ get_id true a2
 || get_id_used (Madda a1 a2)  = get_id true a1 @ get_id true a2

 || get_id_used (Mcall s)  = [(s, true, false)]
 || get_id_used (Mjumpf s) = [(s, true, false)]
 || get_id_used (Mjump s)  = [(s, true, false)]
 || get_id_used (Mjcond _ s)  = [(s, true, false)]
 || get_id_used (Mcompare a1 a2) = get_id true a1 @ get_id true a2
 || get_id_used (Mcomparedf a1 a2) = get_id true a1 @ get_id true a2
 || get_id_used (Mcomparesf a1 a2) = get_id true a1 @ get_id true a2
 || get_id_used (Mop2 _ a1 a2) = get_id true a1 @ get_id true a2
 || get_id_used (Mop3 _ a1 a2 a3) = get_id true a1 @ get_id true a2 @ get_id true a3
 || get_id_used (Mcase a _ _ _ _ x) = ('L'.itos x@"_2", true, true) . get_id true a
 || get_id_used (Mboolcc _ a) = get_id true a
 || get_id_used (Mword a) = get_id false a
 || get_id_used (Masm s l) = (get_id_asm s l
      where rec
         get_id_asm "" _ = []
      || get_id_asm ('^'.cs) (am.ams) = get_id true am @ get_id_asm cs ams
      || get_id_asm ('~'.cs) (glob i.ams) = (fixclbl i, true, true) . get_id_asm cs ams
      || get_id_asm (c.cs) ams = get_id_asm cs ams)
 || get_id_used _ = []

and fixclbl ('_'.s) = '_'.'.'.s

and get_id_defd (Mlabel ('_'.s)) = ['_'.s ; '_'.'.'.s]
 || get_id_defd (Mlabel s) = [s]
 || get_id_defd (Mcase a _ _ _ _ x) = ['L'.itos x@"_2"]
 || get_id_defd _ = []

-- Make a wild guess if it's a data or text reference
and textref s =
	case s in
	   'C' . _ : false
	|| 'V' . _ : false
	|| 'U' . 'S' . 'E' . '_' . _ : false
	|| 'J' . _ : true
	|| 'S' . 'T' . 'R' . 'I' . 'N' . 'G' . _ : false
	|| 'S' . 'Y' . _ : false
	|| 'S' . _ : true
	|| 'u' . 'n' . 'w' . _ : true
	|| 'v' . 'u' . 'n' . 'w' . _ : true
	|| _       : /*trace s*/ false
	end

and extern_declarations ms = ext [] Empty ms
and ext defd used [] =
    let rec uses  = tree2list used
    and     defd' = omkset defd
    and     used' = uses
    and     exts  = odifference used' defd'
    and     doglob s =
	      case find s used in
		(ds, tc) :
		let e = omem s exts in
		let s' = fixlabel s
		and u = if e then if ds then "[DS]" else "[RW]" else ""
		in  (if e then [Masm ("\t.extern\t"@s') []] else []) @
		    (if tc then
			(if e then [Masm ("\t.extern\t"@s'@u) []] else []) @
			[Masm ("LC.."@s'@":\n\t.tc\t"@s'@"[TC],"@s'@u) []] 
		     else [])
	      end
    in      Masm "\t.toc" [] . concmap doglob used'
||  ext defd used (m.ms) = 
	case additems (get_id_used m) used in -- make it strict
	    Empty : m . ext (get_id_defd m @ defd) Empty ms
	||  t : m . ext (get_id_defd m @ defd) t ms
	end

and type Tree = Empty + Node Tree String Bool Bool Tree
and additems [] t = t
||  additems ((x,y,z).xys) t = additems xys (additem x y z t)
and additem x y z Empty = Node Empty x y z Empty
||  additem x y z (t as (Node l x' y' z' r)) = 
	if x<x' then
	    case additem x y z l in
		Empty : fail "additem 1"
	    ||  l' : Node l' x' y' z' r
	    end
	else if x>x' then
	    case additem x y z r in
		Empty : fail "additem 2"
	    ||  r' : Node l x' y' z' r'
	    end
	else
	    if z & ~z' then Node l x' y' true r else t
and tree2list Empty = []
||  tree2list (Node l x _ _ r) = tree2list l @ (x . tree2list r)
and find x Empty = fail ("find")
||  find x (Node l x' y z r) =
	if x<x' then find x l
	else if x>x' then find x r
	else (y, z)
-- remove _, switch from $ to .
and fixlabel ('_'.s) = fixlabel' s
||  fixlabel s = fixlabel' s
and fixlabel' s = map (\x.if x='$' then '.' else x) s

and globvar s = "LC.."@fixlabel s@"(2)"
end
