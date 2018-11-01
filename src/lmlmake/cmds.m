/*
load "cmds";
*/
module	-- cmds - shell command printer
#include "env.t"
#include "argv.t"
#include "filenames.t"
#include "utils.t"
export ifolder,compile,link,parallelcmds;

#if amiga

rec ifolder f fs cmd = "If eq 1 val `$OLDER " @ f @ concmap (' '.) fs @ "`\n  " @ cmd @ "\nEndIf\n"

and compile src = ee (mixsp [compiler;"-c";compilerFlags;executableFileName src])

and link exe objs =
  ee (mixsp ([compiler;compilerFlags;"-l";exe;"obj \""] @ objs @ ["\" $LDFLAGS"]))

and parallelcmds = conc -- no use to compile in parallel...

and ee cmd = (if beQuiet
	      then ""
	      else "echo \"" @ escq cmd @ "\"\n") @ cmd

and escq = concmap (\q.if q='\"' then "*\"" else [q])

#else

#if 0
-- csh version:
rec ifolder f fs cmd = "if (`$OLDER " @ f @ concmap (' '.) fs @ "`) then\n" @ cmd @ "\nendif\n"
#else
-- sh version:
rec ifolder f fs cmd =
      "if [ `$OLDER " @ mixsp (f.fs) @ "` = 1 ]\nthen\n" @ cmd @ "\nfi\n"
#endif

and compile src = /*trace (src@" "@specialFlags src)*/ (ec o mixsp) ([compiler;"-c";compilerFlags;specialFlags src;src])

and link exe objs =
  (el o mixsp) ([compiler;compilerFlags;"-o";exe] @ objs @ ["$LDFLAGS"])

and parallelcmds =
  let parcmd cmds = if length cmds <= 1
		    then cmds
		    else map bg cmds @ [wait]
  and distrcmd [] = []
   || distrcmd cmds = emptycmds.cmds @ [execmds]
  in if distributeFlag
     then concmap distrcmd
     else if parallelFlag
          then concmap parcmd
	  else conc

and wait = "wait"
and bg cmd = cmd @ " &"
and quote cmd = "'" @ cmd @ "'"

and emptycmds = "cmds=\"\""
and conscmds cmd = "cmds=\"$cmds '" @ cmd @ "'\""
and execmds = "eval distcom $cmds"

and ec = ee true
and el = ee false

and ee d cmd = (if beQuiet
	      then ""
	      else "echo " @ cmd @ "\n") @ 
	      (if distributeFlag & d
	       then conscmds cmd
	       else cmd)

#endif

and mixsp xs = mix xs " "

end
