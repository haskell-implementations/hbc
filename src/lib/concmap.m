/*
**	concmap:	concatenates lists obtained from f mapped onto a list.
**
**		concmap f [x1; ... xn] = f x1 @ ... @ f xn
*/
module
export	concmap;
rec

   concmap f [] = []
|| concmap f (a.b) = f a @ concmap f b
end
