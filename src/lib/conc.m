/*
**	conc:		concatenates elements in a list of lists.
**
**		conc [ l1; ... ; ln ] = l1 @ ... @ ln
*/
module
export	conc;
rec

   conc [] = []
|| conc ([].xss) = conc xss	-- avoid stack leak
|| conc (xs.xss) = xs @ conc xss
end
