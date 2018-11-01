/*
**	split:		split a list of pairs into a pair of lists.
*/
module
export	split;
rec

   split [] = ([],[])
|| split ((x1,x2).l) =
	let
		(l1, l2) = split l
	in
		(x1.l1, x2.l2)
end
