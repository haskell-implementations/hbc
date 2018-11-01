/*
**	iscntrl:	determine if a character is a control-character.
*/
module
export	iscntrl;
iscntrl c = ~(c >= ' ' & c <= '~' | c >= '\240' & c <= '\377')
end
