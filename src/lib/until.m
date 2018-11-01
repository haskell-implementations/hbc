/*
**	until:		apply g on x (1 or more times) until f x is true.
**
**		until f g x
*/
module
export	until;
rec

until f g x =
		let
			x1 = g x
		in
			if f x1 then
				x1
			else
				until f g x1
end
