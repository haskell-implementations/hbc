/*
**	mix:		takes a list and an item and concatenates the elements
**			in the list with an item between every element.
**
**		mix ["2";"hello";"*"] "," gives "2,hello,*"
*/
module
export	mix;
rec

    mix []    x = []
 || mix (a.b) x = a @ case b in
		         [] : []
		      || _  : x @ mix b x
		      end
end
