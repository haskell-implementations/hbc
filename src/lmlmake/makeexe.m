/*
load "makeexe";
*/
module	-- makeexe -
#include "filenames.t"
#include "cmds.t"
export makeExecutables;

rec makeExecutables g ms = map (mkexe (conc g)) (filter isexe ms)

and isexe f = f = executableFileName f

and mkexe g f =
  let objs = modules g [] [f]
  in ifolder f objs (link f objs)

and modules g visited [] = []
 || modules g visited (f.fs) =
	let obj=objectFileName f
	in
	if mem obj visited
	then modules g visited fs
	else let deps = depmods g obj
	     in obj.modules g (obj.visited) (fs @ deps)

and depmods g obj =
  concmap (filter isInterfaceFileName o map fst o snd) (filter ((=obj) o fst o fst) g)

end
