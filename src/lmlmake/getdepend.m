/*
load "getdepend";
*/
module	-- getdepend - extact dependencied from source files.
#include "fs.h"
#include "filenames.t"
#include "utils.t"
#include "env.t"
export extractDependencies,FileId;
-- FileId should be abstract, but isn't because of bad Haskell interface files

rec extractDependencies = extr [] []

-- extr does a breadth first traversal of the dependecy graph...
and extr visited deps modules =
  case modules
  in []: (visited,consistentnames visited deps)
  || m.ms:
	let (src,srcId) = srcFor m
	and obj = objectFileName m
	in let srcDir = dirName src
	in if isvisited srcId visited
	   then extr (addalias srcId src visited) deps ms
	   else let imports = expandImports src
		in let newmodules = filter isInterfaceFileName imports
	        in extr (visit srcId src visited) ((obj,(src.imports)).deps) (ms @ newmodules)
  end

and extractImports src =
  case openfile src
  in No err: missingSource src err
  || Yes contents: let isliterate = src = litSourceFileName src
#if 1
     -- Trying to work around a file descriptor limitation on Dec Alphas.
		   in let imports = parseImports isliterate contents
		   in if length imports<0
		      then fail "length<0"
		      else imports
#else
                   in parseImports isliterate contents
#endif
  end

and srcFor modname =
  if isSourceFileName modname
  then pairwith fileId modname
  else case filter (not o isnonexistent o snd)
		   (map (pairwith fileId)
			[litSourceFileName modname; sourceFileName modname])
       in src._: src
       || _: (modname,NonExistent modname)
       end

and missingSource name err = fail ("Missing source file: " @ name @ " (" @ err @ ")")

and expandImports src = concmap (expandIncludes (dirName src)) (extractImports src)
--and expandImports src =  (extractImports src)

and expandIncludes srcDir importfilename =
  if isInterfaceFileName importfilename
  then [findInclude (snd o srcFor) srcDir importfilename]
		-- An ordinary interface file is imported
  else let realImportPath = findInclude fileId srcDir importfilename
       in realImportPath.expandImports realImportPath
		-- A ".h" file is imported, check it for further #includes
		-- (relevant for LML, but not for Haskell)

and type FileId = NonExistent String + Inode  Int Int

and isnonexistent (NonExistent _) = true
 || isnonexistent _ = false

and fileId name =
  case statfile name
  in Yes (dev.inode._): Inode dev inode
  || No _: /*trace ("nonexistent file: " @ name)*/ (NonExistent name)
  end

and isvisited id vs = mem id (map fst vs)

and addalias id name [] = [(id,[name])] -- this case not used
 || addalias id name ((id',ns).vs) =
	if id=id'
	then (id',if mem name ns then ns else name.ns).vs
	else (id',ns).addalias id name vs

and visit id name vs = (id,[name]).vs

#if 0
#define HD (hd1 __FILE__ __LINE__)
and hd1 f l [] = fail ("hd on [], " @ f @ ", line " @ itos l)
 || hd1 _ _ (x._) = x
#else
#define HD hd
#endif

and consistentnames visited deps =
  let mods = map (HD o snd) deps
  and srcs = map snd visited
  in let cn name =
	if isInterfaceFileName name
	then case filter (mem (sourceFileName name)) srcs
	     in []: name	-- interface file without source !!
	     || (syns._): interfaceFileName (HD (filter (\m.mem m syns) mods))
	     end
	else name
  in map (asnd (map cn)) deps

and findInclude f srcDir name =
  --let f s = let t=f s in trace (show_pair (show_string,show_bool) (s,isnonexistent t)) t in
  if isAbsolute name then name
  else if includePath=[] then joinPath srcDir name
  else case [dir;;dir<-srcDir.includePath;not (isnonexistent (f (joinPath dir name)))]
       in []: joinPath srcDir name --  name wasn't found in include path !!
       || dir._: joinPath dir name
       end

and fileExists name =
  case fileId name
  in NonExistent name: false
  || _ : true
  end

-- Useful for debugging
--and tr s x = trace (s x) x

end
