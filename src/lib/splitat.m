/*
**	splitat:	split a list at item i,
**			return a pair, the list of items before i and
**			the list of items after i, dropping i altogether.
**
**		splitat 3 [1;2;3;4;5] = ([1;2],[4;5])
*/
module
export	splitat;
rec

   splitat c [] = ([], [])
|| splitat c (a.b) = if a = c then ([], b)
			      else let (x, y) = splitat c b
				   in (a.x, y)
end
