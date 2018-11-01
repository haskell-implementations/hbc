/*
**	ispunct:	determine if a character is a punctuation character.
*/
module
export	ispunct;
ispunct c = (c >= ' ' & c <= '~' | c >= '\240' & c <= '\377') & ~(
            ord c >= ord 'a' & ord c <= ord 'z' |
            ord c >= ord 'A' & ord c <= ord 'Z' |
	    ord c >= ord '0' & ord c <= ord '9'
            )
end
