module
export BT, show_BT, btsimpl, bttt, btff, substbt; --remeq, cross, btsimpl, inserta, inserto, fixup, sortd;
rec type BT = btvar Int + btands (List BT) + btors (List BT) + btapp Int (List BT) + btfunarg Int Int
and show_BT (btands []) = "tt"
||  show_BT (btands [btors []]) = "ff"
||  show_BT (btors []) = "ff"
||  show_BT (btvar v) = 'v'.itos v
--||  show_BT (btands [x]) = show_BT x
||  show_BT (btands xs) = "(and "@mix (map show_BT xs) " "@")"
--||  show_BT (btors [x]) = show_BT x
||  show_BT (btors xs) = "(or "@mix (map show_BT xs) " "@")"
||  show_BT (btapp f xs) = "(f"@itos f@"("@mix (map show_BT xs) ", "@"))"
||  show_BT (btfunarg f a) = "f"@itos f@"_"@itos a

and remeq (x.(xs as y._)) = if x=y then remeq xs else x.remeq xs
||  remeq xs = xs

and insert x [] = [x]
||  insert x (y.ys) = if x < y then x.y.ys else y.insert x ys
and inserto (btors x) (btors xs) = btors (sortd (x@xs))
and inserta xs x = map (inserto x) xs

and cross (btands xs.xss) =
    let xss' = cross xss in
    concmap (inserta xss') xs
||  cross [] = [btors []]

and sortd l = remeq (sort (<) l)

and subsume t ts = if exists (impliedby t) ts then ts else t.ts
and impliedby (btors vs1) (btors vs2) = subset vs2 vs1
and subset [] _ = true
||  subset (xxs as x.xs) (y.ys) =
    if x = y then subset xs ys
    else if x < y then false
    else subset xxs ys
||  subset (_._) [] = false

and fixup ws =
--let rrr =
    btands (sort (<) (reduce subsume [] (sort (\ (btors x).\ (btors y).length x > length y) ws)))
#if 0
in
let r1 = (sort (\(btors x).\(btors y).length x > length y) ws) in
let r2 = (reduce subsume [] r1) in
let r3 = sort (\x.\y.trace ("compare "/*@show_BT x@" < "@show_BT y@" => "@show_bool (x<y)*/) (x < y)) r2 in
let r4 = btands r3 in
trace ("fixup ws="@/*show_list show_BT ws@" r1="@show_list show_BT r1@" r2="@show_list show_BT r2@" r3="@show_list show_BT r3@*/" r4="@show_BT r4) r4
#endif

and btff = btands [btors []]
and bttt = btands []
and btsimpl (v as btvar _) = btands [btors [v]]
||  btsimpl (v as btfunarg _ _) = btands [btors [v]]
||  btsimpl (btands xs) = 
    fixup (concmap (\x.let (btands ys) = btsimpl x in ys) xs)
||  btsimpl (btors xs) =
    let ws = map btsimpl xs in
    fixup (cross ws)
||  btsimpl (btapp f xs) = btands [btors [btapp f (map btsimpl xs)]]

and substbt l (btands xs) = btands (map (subor l) xs)
and subor l (btors vs) = btors (map (subvar l) vs)
||  subor l bt = fail ("subor "@show_list show_BT l@show_BT bt)
and subvar l (btvar n) = select (n+1) l
||  subvar l (btapp f xs) = btapp f (map (substbt l) xs)
||  subvar l (b as btfunarg _ _) = b
end

