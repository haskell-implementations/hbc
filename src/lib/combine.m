/*
**	combine:	combines a pair of lists to a list of pairs.
**
**		combine ([x1; ... xn], [y1; ... yn]) = [(x1;y1); ... (xn; yn)]
*/
module
export	combine;
rec

   combine (x1.l1, x2.l2) = (x1,x2) . combine (l1,l2)
|| combine (_,_) = [] -- Ends when any of the lists ends

end
