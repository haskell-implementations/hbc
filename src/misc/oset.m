module
export omkset, oremdup, omem, ounion, ointersect, odifference,
       omksetby, oremdupby, omemby, ounionby, ointersectby, odifferenceby;
-- Ordered sets

rec omkset (s as (_._._)) = oremdup (sort (<) s)
||  omkset s = s

and oremdup (x1.(xs as (x2._))) = if x1=x2 then oremdup xs else x1.oremdup xs
||  oremdup xs = xs

and omem x  []   = false
 || omem x (y.l) = x = y | x > y & omem x l

and ounion   A           []        = A
 || ounion   []          B         = B
 || ounion (aA as a.A) (bB as b.B) =  if a = b then a.ounion A  B
				 else if a < b then a.ounion A  bB
				 else /* a > b */   b.ounion aA B
and ointersect   A           []   = []
 || ointersect   []          B    = []
 || ointersect (aA as a.A) (bB as b.B) = if a = b then a.ointersect A  B
				    else if a < b then   ointersect A  bB
				    else /* a > b */     ointersect aA B
and odifference  A    []   = A
 || odifference  []   B    = []
 || odifference (aA as a.A) (bB as b.B) = if a = b then  odifference A  B
				     else if a < b then a.odifference A  bB
				     else /* a > b */     odifference aA B

and omksetby eq lt (s as (_._._)) = oremdupby eq lt (sort lt s)
||  omksetby _  _ s = s

and oremdupby eq lt (x1.(xs as (x2._))) = if eq x1 x2 then oremdupby eq lt xs else x1.oremdupby eq lt xs
||  oremdupby _  _  xs = xs

and omemby _ _ x  []   = false
 || omemby eq lt x (y.l) = eq x y | lt y x & omemby eq lt x l

and ounionby eq lt   A           []        = A
 || ounionby eq lt   []          B         = B
 || ounionby eq lt (aA as a.A) (bB as b.B) =  
     				 if eq a b then a.ounionby eq lt A  B
				 else if lt a b then a.ounionby eq lt A  bB
				 else /* a > b */   b.ounionby eq lt aA B
and ointersectby eq lt   A           []   = []
 || ointersectby eq lt   []          B    = []
 || ointersectby eq lt (aA as a.A) (bB as b.B) = 
     				    if eq a b then      a.ointersectby eq lt A  B
				    else if lt a b then   ointersectby eq lt A  bB
				    else /* a > b */      ointersectby eq lt aA B
and odifferenceby eq lt  A    []   = A
 || odifferenceby eq lt  []   B    = []
 || odifferenceby eq lt (aA as a.A) (bB as b.B) = 
				     if eq a b then        odifferenceby eq lt A  B
				     else if lt a b then a.odifferenceby eq lt A  bB
				     else /* a > b */      odifferenceby eq lt aA B

end
