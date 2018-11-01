/*
**	islower:	determine if a character is a lower-case letter.
*/
module
export	islower;

islower c = ord c >= ord 'a' & ord c <= ord 'z'
end
