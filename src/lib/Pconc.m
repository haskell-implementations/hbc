/*
**	Pconc:		lml part of @
*/
module
export (@);
rec (@) []       _ys = _ys
||  (@) (_x._xs) _ys = _x. (_xs @ _ys)
end
