/*
**	isalnum:	determine if a character is a letter or digit.
*/
module
export	isalnum;

isalnum c = ord c >= ord 'a' & ord c <= ord 'z' |
            ord c >= ord 'A' & ord c <= ord 'Z' |
	    ord c >= ord '0' & ord c <= ord '9'
end
