/*
**	lex:		returns a function that compares lists lexicografically
**			given a function that compares elements.
**
**		lex (\x.\y.x<y) [1;2;2] [1;2;3] = true
*/
module
export lex;
rec

   lex lt [] [] = false
|| lex lt [] (c.d) = true
|| lex lt (a.b) [] = false
|| lex lt (a.b) (c.d) = lt a c | ~(lt c a) & lex lt b d
end
