/*
load "makemod";
*/
module	-- makemod -
#include "getmodtime.t"
#include "cmds.t"
#include "filenames.t"
#include "utils.t"
export makeModules;

rec makeModules = parallelcmds o snd o mapstate mkcommands []

and mkcommands chts [] = (chts,[])
 || mkcommands chts (m.ms) =
	case mkcommand chts m
	in None: mkcommands chts ms
	|| Some cmd: asnd (cmd.) (mkcommands (tname (fst m).chts) ms)
	end

and mkcommand changed_ts ((obj,tobj),((src,tsrc).tts)) =
  if outofdate tobj (tsrc.map snd tts)
  --if outofdate (obj,tobj) ((src,tsrc). tts)
  then Some (compile src)
  else case intersect changed_ts (map fst tts)
       in []: None
       || ts: Some (ifolder obj ts (compile src))
       end

#if 1
--normal version
and outofdate tobj ts = Or (map (isOld tobj) ts)

#else

and outofdate tobj ts =
  let newer = (filter (isOld (snd tobj) o snd) ts)
  in trace (show_file tobj @ " is older than " @ mix (map show_file newer) " ")
      (not (null newer))

and show_file (name,time) = name @ "(" @ show_When time @ ")"

#endif

and tname (t,_) = interfaceFileName t


end
