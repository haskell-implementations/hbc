/*
**	isprint:	determine if a character is a printing character.
*/
module
export	isprint;

isprint c = c >= ' ' & c <= '~' | c >= '\240' & c <= '\377'
end
