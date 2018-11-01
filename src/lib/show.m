/*
**	show:
**	Show family:	convert a value to a string
**	show_int i	show an integer
**	show_bool b	show a boolean
**	show_char c	show a char
**	show_string s	show a list of chars
**	show_list f l	show a list, elements showed with f
**	show_pair (f,g) p show a pair, components showed with f and g
**			Example: show a list of pairs, the pair is a boolean
**			and an integer:
**	show_list (show_pair (show_bool, show_int)) [(true, 5); (false,-1)] =
**	"[(true, 5); (false,-1)]"
*/
module
-- WARNING: not self contained
export	show_int, show_bool, show_char, show_string,
	show_ok, show_option,
	show_list, show_pair, show_array;
rec

    show_int n = itos n
and show_bool true = "true"
 || show_bool false = "false"
and show_char '\'' = "'\\''"
 || show_char '\n' = "'\\n'"
 || show_char '\t' = "'\\t'"
 || show_char c = ['\'';c;'\'']
and show_string s = '"'.concmap (\x.if x = '"' then "\\\"" else [x]) s @ "\""
and show_list f l = '['.mix (map f l) "; " @ "]"
and show_pair (f, g) (a,b) = "(" @ f a @ ", " @ g b @ ")"
-- show_array will not compile with old compiler
#ifndef USELIST
and show_array f a = 
	"{("@show_int (lowerbound a)@","@
	     show_int (upperbound a)@"); "@
        mix [f (a?i);;i<-[lowerbound a .. upperbound a]] "," @ "}"
#endif
and show_ok f g (No n) = "(No "@f n@")"
||  show_ok f g (Yes y) = "(Yes "@g y@")"
and show_option f None = "None"
||  show_option f (Some s) = "(Some "@f s@")"
end
