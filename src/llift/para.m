module -- para
#include "../expr/id_t.t"
#include "../expr/einfo_t.t"
#include "../ExprE/Expr_t.t"
export paraspark;
rec
    elet r [] e = e
||  elet r ds e = Elet r ds e
and edoeval [] e = e
||  edoeval is e = Einfo (doeval is) e
and edospark [] e = e
||  edospark is e = Einfo (spark is) e
and dropev (i, Einfo strict e) = (i, e)
and sparkable e = true
and paraspark bs is e =
	let s = filter sparkable bs
	and ns = filter ((~) o sparkable)  bs
	in
		if length s < 2 then
			edoeval is (elet false bs e)
		else
			let (d.ds) = s in
			let sis = map fst ds in
			elet false (map dropev ds) (edospark sis (
			(edoeval is (elet false (d.ns) (edoeval sis e)))))
end
