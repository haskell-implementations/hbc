/*
**	isdigit:	determine if a character is a digit.
*/
module
export	isdigit;

isdigit c = ord c >= ord '0' & ord c <= ord '9'
end
