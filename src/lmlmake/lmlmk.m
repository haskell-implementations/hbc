#include "getdepend.t"
#include "getmodtime.t"
#include "makemod.t"
#include "makeexe.t"
#include "order.t"
#include "filenames.t"
#include "utils.t"
#include "argv.t"

let show_file (name,time) = name @ (if isopt "b"
				    then "(" @ show_When time @ ")"
				    else "")
in let show_dep (obj,ts)= show_file obj @ ": " @ mix (map show_file ts) " "
in let show_deps = map show_dep
in let modificationTimes = (map o apair o pairwith map o pairwith) filemodtime
in let strip = map (apair (pairwith map objectFileName) o asnd tl)
in let order' g = (decorate g o scctsort o closegraph o strip) g
in let build_graph = asnd (map modificationTimes) o order' o snd o extractDependencies
in let visited = fst o extractDependencies
and show_names = map (\ns.mix (map (fst o fst) ns) " ")
in let show_cycles [] = []
    || show_cycles cys =[TOSTDERR] @ "Cyclic dependencies:\n" @
			mklines (map (\y.mix y " ") cys) @ [TOSTDOUT]
in if modules=[]
   then TOSTDERR."Usage: lmlmk [+b] [+d] [+g] [+t] target ...\n"
   else let (cycles,timedg) = build_graph modules
	in let objcmds = makeModules timedg
	   and execmds = makeExecutables timedg modules
	in ifdebug ["Depencency graph:"] @
	   ifopt ["g"] (concmap show_deps timedg) @
	   ifdebug ["";"tsorted dependencies:"] @
	   ifopt ["t"] (show_names timedg) @
	   show_cycles (filter ((>1) o length) cycles) @
	   ifdebug ["";"Commands:"] @
	   ifnotopt ["t";"g"] (objcmds @ execmds) @
	   ifdebug ("Visited=".map (\(_,ns).mix ns " ") (visited modules))
