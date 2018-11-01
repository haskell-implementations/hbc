/*
**	itos:		convert an integer to a string.
*/
module
export	itos;
rec
itos n = 
    if n < 0 then
	if -n < 0 then
	    /* we have minint, a difficult number */
	    itos (n/10) @ itos (-(n%10))
	else
	    '-'.itos1 (-n)
    else 
	itos1 n
and itos1 n = 
    if n < 10 then
	[chr (n + ord '0')]
    else 
	itos1 (n/10) @ [chr (n%10+ord '0')]
end
