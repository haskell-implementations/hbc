/*
**	tsort:		topological sort
**
**		tsort G	sorts the graph G.  A graph is a list of nodes, each
**		node is a pair, a name and a list of names of connected nodes.
*/
module
-- WARNING: not self contained
export tsort;
rec
    tsort [] = []
||  tsort G  =
	case partition (\(_,x).null x) G in
	   ([], _) : fail "tsort: cycle in data"
	|| (a, b)  : let a' = map fst a in
			a @ tsort (map (\(x, xs).(x, difference xs a')) b)
	end
end
