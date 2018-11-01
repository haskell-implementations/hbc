/*
**	ptsort:		partial topological sort
**
**		tsort G	sorts the graph G.  A graph is a list of nodes, each
**		node is a pair, a name and a list of names of connected nodes.
**
**		The output is a list of lists, where the lists contain nodes
**		that come before nodes occuring in later lists.
**		The order of nodes within each list is arbitrary and not
**		determined by the graph.
*/
module
export ptsort;
rec
    ptsort [] = []
||  ptsort G  =
	case partition (\(_,x).null x) G in
	   ([], _) : fail ("ptsort: cycle in data\n"
			   --@ concmap (\(f,fs).f @ ": " @ mix fs " " @ "\n") G
			  )
	|| (a, b)  : let a' = map fst a in
			a' . ptsort (map (\(x, xs).(x, difference xs a')) b)
		-- was: a  @ ...
	end
end
