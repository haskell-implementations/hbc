/*
load "order";
*/
module	-- order -
#include "utils.t"
#include "tsort.t"
#include "graph.t"
export order,scctsort,decorate,closegraph;

rec order g = (decorate g o scctsort o closegraph) g

and decorate g = asnd (map (map (\f.(f,assoc f g))))

-- scctsort: topological sort of strongly connected components
-- Nodes within a scc will appear in an arbitrary order.
and scctsort g =
  let sccg = scceq (=) g -- compute the strongly connected components
  in let sccs = map (map fst) sccg
  in let cg = map collapse_node sccg -- collapse nodes in a scc to a single node
  in let sortg = (ptsort o
		  rmreflx o closegraph o collapse_graph sccs) cg -- sort it
  in (sccs, map (concmap (reconstruct sccg)) sortg) -- put back all nodes in scc:s

and closegraph g =	-- remove edged leading to nodes outside the graph
  let nodes = map fst g
  in let isnode f = mem f nodes -- is f a node in g?
  in map (asnd (filter isnode)) g

and rmreflx = -- remove reflexivity
  map (\(f,fs).(f,filter (~=f) fs))

and collapse_node ((f,fs).ms) = -- collapse all nodes in a scc to one node
  (f,fs @ conc (map snd ms))

and collapse_graph eq = map (apair (pairwith map (reprnode eq)))

and reconstruct sccg f = assoc f (map (\((f,_).fs).(f,f.map fst fs)) sccg)

and reprnode eq a = (hd o hd o filter (mem a)) eq

end
