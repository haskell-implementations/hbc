/*
**	mapstate:	hybrid between map and revitlist.
**			Takes a state transition function, an initial state,
**			and a list of things.
**			Returns the last state and the list of results of the
**			state transitions.
**			The state transition function, f, should take a state
**			and an object and should return a pair holding the new
**			state and the result.
**			Ignoring the state this is map, ignoring the produced
**			list it is revitlist.
**		mapstate (\x.\e.(x+1,(e,x))) 0 ['a'; 'b'; 'c'] =
**				(3, [('a',0); ('b',1); ('c',2)])
*/
module
export	mapstate;
rec

   mapstate f os []    = (os, [])
|| mapstate f os (h.t) = let (ns, el) = f os h in
			 let (nns, ell) = mapstate f ns t in
			 (nns, el.ell)
end
