/*
**	isspace:	determine if a character is "white".
*/
module
export	isspace;

isspace c = c = ' ' | c = '\n' | c = '\t' | c = '\f' | c = '\r'
end
