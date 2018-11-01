/*
**	stoil:		converts a string to an integer list.
*/
module
export stoil;
rec

   stoil []      = []
|| stoil (x.l)   = 
	let rec f []    n sign = [sign*n]
	     || f (c.l) n sign = if isdigit c then
				   f l (10*n + ord c - ord '0') sign
			        else
				   (sign*n).stoil l
	in
		if x = '-' then
			f l 0 (-1)
		else if isdigit x then
			f (x.l) 0 1
		else
			stoil l
and isdigit c = '0' <= c & c <= '9'
end
