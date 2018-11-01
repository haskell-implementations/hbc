/*
**	head:		get first n elements of a list.
**
**		head 3 [2;6;4;7;1] = [2;6;4]
*/
module
export	head;
rec
   head 0 l = []
|| head n [] = []
|| head n (a.b) = a.head (n-1) b
end
