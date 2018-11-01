/*
**	filter:		extract all the elements of a list for which p holds.
**
**		filter p l = [all x such that: x in l AND p x]
*/
module
export	filter;
rec

   filter p [] = []
|| filter p (a.b) = if p a then a . filter p b
			   else filter p b
end
