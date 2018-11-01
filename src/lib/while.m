/*
**	while:		apply g x (0 or more times) while f x is true.
**
**		while f g x
*/
module
export	while;
rec

while f g x =
		if f x then
			while f g (g x)
		else
			x
end
