/*
**	length:		computes the length of a list.
*/
module
export	length;

   length l = len 0 l
where rec
   len n [] = n
|| len n (a.b)	= len (n+1) b
end
