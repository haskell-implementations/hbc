module
export tsorteq, scceq, diffeq, intereq, unioneq, assocdefeq, allsameeq, anysameeq, mkseteq, hascycleeq, seteq;
rec
    tsorteq eq [] = []
||  tsorteq eq G  =
	case partition (\(_,x).null x) G in
	   ([], _) : fail "tsorteq: cycle in data"
	|| (a, b)  : let a' = map fst a in
			a @ tsorteq eq (map (\(x, xs).(x, diffeq eq xs a')) b)
	end
and hascycleeq eq [] = false
||  hascycleeq eq G  =
	case partition (\(_,x).null x) G in
	   ([], _) : true
	|| (a, b)  : let a' = map fst a in hascycleeq eq (map (\(x, xs).(x, diffeq eq xs a')) b)
	end
and diffeq eq l1 l2 = filter (\x.~ (member eq x l2)) l1
and intereq eq l1 l2 = filter (\x. member eq x l2) l1
and unioneq eq l1 l2 = l1 @ diffeq eq l2 l1
and assocdefeq eq i [] d = d
||  assocdefeq eq i ((k,v).r) d =
	if eq i k then
		v
	else
		assocdefeq eq i r d

and scceq eq G = (
	(let (low, cs) = mapstate g [] G
	    where g low (vv as (v, _)) =
		if assocdefeq eq v low 0 = 0 then
			let ((n,low,stack,min), cs) = searchc 1 low vv
			in (low, cs)
		else
			(low, [])
	in
	conc cs)
    where rec
    searchc n low (vv as (v,es)) =
	let n = n+1 in
	let low = (v,n).low in
	let ((n,low,nstack,min),cs) = mapstate f (n,low,[vv],n) es
    	where rec f (n,low,stack,min) w =
    	    let ((n,low,stack',m),cs) =
	    	let vm = assocdefeq eq w low 0 in
	    	if vm = 0 then
		    searchc n low (w, assocdefeq eq w G (fail "scc-assoc"))
	        else
		    ((n,low,[],vm),[])
	    in
	    ((n,low,stack'@stack,if m<min then m else min), cs)
	in
	let cs = conc cs in
	if assocdefeq eq v low (fail "scc-assoc") = min then
		((n,map (\(x,_).(x,maxint)) nstack @ low,[],min), cs@[nstack])
	else
		((n,low,nstack,min), cs)
	)
and allsameeq _ [] = true
 || allsameeq _ [a] = true
 || allsameeq eq (a.b) = all (eq a) b
and anysameeq _ [] = false
 || anysameeq eq (a.b) = member eq a b | anysameeq eq b
and mkset' eq l []	   = []
 || mkset' eq l (a.b) = if member eq a l then mkset' eq l b else a.mkset' eq (a.l) b
and mkseteq eq l = mkset' eq [] l
and seteq eq x y = null (diffeq eq x y) & null (diffeq eq y x)
end

