/*
**	automat:	Applies a state transition function on a list of
**			arguments, producing a list of answers.
**			Works like mapstate but does not return the state.
**
**		automat func state arglist
*/
module
export	automat;
rec

   automat func state [] = []
|| automat func state (x.xs) =
	let
		(newstate , answer) = func state x
	in
		answer . automat func newstate xs
end
