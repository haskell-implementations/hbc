/*
load "filenames";
*/
module	-- filenames - extension and pathname manipulation
#include "env.t"
export	objectFileName,sourceFileName,interfaceFileName,executableFileName,
	litSourceFileName,
	isSourceFileName,isInterfaceFileName,isAbsolute,
	dirName,splitPath,joinPath;

rec
    splitExt name =
	let (ext1,base1) = splitat '.' (rev name)	 -- split at last '.'...
	in let (rbase,rext) = if base1=""
		              then (ext1,"")	-- ... or at end if no '.'
		              else (base1,ext1)
	in (rev rbase,'.'.rev rext)

and basename = fst o splitExt
and extension = snd o splitExt

and replaceExtension newext path =
	let (dir,name) = splitPath path
	in joinPath dir (basename name @ newext)

and objectFileName = replaceExtension ".o"
and sourceFileName = replaceExtension srcExt
and litSourceFileName = replaceExtension litSrcExt
and interfaceFileName = replaceExtension ifaceExt
and executableFileName = replaceExtension ""

and isAbsolute ('/'._) = true
 || isAbsolute _ = false

and isInterfaceFileName name = extension name = ifaceExt
and isSourceFileName name = mem (extension name) [srcExt;litSrcExt]

and dirName = fst o splitPath

and splitPath pathname =
	let name.path = rev (choplist (splitat '/') pathname)
	in (case rev path
	    in []: ""	-- or "./"
	    || ps: concmap (@"/") ps
	    end,
	    name)

-- compactPath removes "unessessary" .. from paths. This assumes that no
-- symbolic links interfere. You need this if you use paths as file
-- identification. It's better to use inode numbers!
-- "x/../y" is equal to "y", unless x is "." or ".." or x is a symbolic link
-- to a directory with a different parent...
and compactPath =
  let rec compact [] = []
       || compact [x] = [x]
       || compact ("..".x.xs) & (x~=".." & x~=".")=compact xs
       || compact (x.xs)=x.compact xs
  in (\p.mix p "/") o rev o compact o rev o choplist (splitat '/')

and joinPath dir file = compactPath (dir @ file)
	-- joinPath works correctly if the parts were obtain with splitPath

#if 0
and concPath path1 path2 =
  if isAbsolute path2
  then path2
  else if path1="." | path1=""
       then path2
       else path1 @ "/" @ path2
#endif

end -- module
