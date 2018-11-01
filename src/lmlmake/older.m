#include "getmodtime.t"

case argv
in [] : fail "Usage: older file.o file1.t ... filen.t"
|| fs :
	let (tobj.ts) = map filemodtime fs
	in if Or (map (isOld tobj) ts)
	   then "1\n"
	   else "0\n"
end
