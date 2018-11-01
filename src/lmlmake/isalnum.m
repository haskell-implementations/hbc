/*
**	isalnum:	determine if a character is a letter or digit.
*/
module
#include "isalpha.t"
export	isalnum;

isalnum c = isalpha c |
	    ord c >= ord '0' & ord c <= ord '9'
end
