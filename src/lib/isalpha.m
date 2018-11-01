/*
**	isalpha:	determine if a character is a letter.
*/
module
export	isalpha;

isalpha c = ord c >= ord 'a' & ord c <= ord 'z' |
            ord c >= ord 'A' & ord c <= ord 'Z'
end
