/*
**	isalpha:	determine if a character is a letter.
*/
module
export	isalpha;

isalpha c = ord c >= ord 'a' & ord c <= ord 'z' |
            ord c >= ord 'A' & ord c <= ord 'Z' |
            ord c >= 0xc0    & ord c <= 0xd6    |
            ord c >= 0xd8    & ord c <= 0xde    |
            ord c >= 0xdf    & ord c <= 0xf6    |
            ord c >= 0xf8    & ord c <= 0xff
end
