module	-- env -
#include "fs.h"
#include "utils.t"
#include "unlit.t"
#include "isalpha.t" /* used by takeword */
#include "isalnum.t" /* used by takeword */
#if 0
export srcExt,litSrcExt,ifaceExt,parseImports,compiler,compilerFlags,parallelFlag,
	distributeFlag,includePath,specialFlags;
#endif

rec [srcExt;litSrcExt;ifaceExt;importKind;compiler;compilerFlags;libraryPathVar] =
  let ps = [("lml",[".m";".lm";".t";"cpp";lmlc;"$LMLFLAGS";"LMLINCPATH"]);
	    ("hbc",[".hs";".lhs";".hi";"haskell";hbc;"$HBCFLAGS";"HBCINCPATH"]);
	    ("um",[".um";".lum";".tum";"haskell";"/u/sparud/com/umc";"$UMCFLAGS";"UMCINCPATH"])]
  in getenvf chop "LMLMK_CONFIG" (assoc (getenvdef "LMLMK_LANG" "lml")  ps)
  --in ts (getenvf chop "LMLMK_CONFIG" (assoc (getenvdef "LMLMK_LANG" "lml")  ps))

#if 0
  in let default = assoc "lml" ps
  in case getenv "LMLMK_LANG"
     in Yes lang: assocdef lang ps default
     || No _: default
     end
#endif

#if 0
and tsl x = trace (show_list show_string x) x
and ts x = trace (show_string x) x
#endif

--and hbc = getenvdef "HBC" "hbcmakehbc"
and hbc = getenvdef "HBC" "hbc"
and lmlc = getenvdef "LMLC" "lmlc"

and getflag s =
  case getenv s
  in Yes _ : true
  || No _ : false
  end

and parseImports =
  assocdef importKind [("cpp",lmlImport);("haskell",hbcImport)]
	   (fail ("unknown import kind: " @ importKind))

and parallelFlag = getflag "PARALLEL"
and distributeFlag = getflag "LMLMKDISTR"

-- These parsers are just fast hacks. It would be nice to have a compiler
-- option to output the import list of a module.
and lmlImport literate =
	--(\x.trace (show_list show_string x) x) o
	map fst o
	filter (not o mem "lmlmake_ignore" o choplist takeword o snd) o
	map (splitat '"' o tl) o
	filter ((='"') o hd) o
	map snd o
	filter ((="include") o fst) o
	map (splitat ' ' o unindent o tl) o
	filter ((='#') o hd) o
	fst o take (not o is_one_of ["export"; "rec"; "let"]) o
	filter (~="") o
	map unindent o
	(if literate then unlit else id) o
	lines

and specialFlags src =
  case openfile src
  in No _: fail src
  || Yes c: (dropTo " Flags:" o fst o take (~='\n')) c
  end

and dropTo sub t = 
    let rec
	d1 s as (_.ts) = 
	let rec
	    dd (r.rs) (x.xs) & (r=x) = dd rs xs
	||  dd (r.rs) (x.xs) = d1 ts
	||  dd _ xs = xs
	in dd sub s
    ||  d1 [] = []
    in d1 t

and is_one_of ss s = Or (map (prefixin s) ss)

and prefixin _ [] = true
 || prefixin (x.xs) (y.ys) = x=y & prefixin xs ys
 || prefixin _ _ = false

and hbcImport literate =
	map (@ifaceExt) o
	filter notFromHbcLibrary o
        filter notPrelude o
	map (hd o rmqualified) o
	filter (not o null) o
	map tl o
	filter ((="import") o hd) o
	map (choplist takeword) o
        --filter ((='i') o hd) o -- gives no speedup
	filter (~="") o
	map unindent o
	(if literate then unlit else id) o
	lines

and rmqualified (wws as w.ws) =
      if w="qualified" then ws else wws		-- Haskell 1.3

and notPrelude name =
  let prelude="Prelude"
  in head (length prelude) name~=prelude

and notFromHbcLibrary name = 
  inPath sourcePath (name@srcExt) |
  inPath sourcePath (name@litSrcExt) |
  not (inPath libraryPath (name@ifaceExt))

and inPath path name = [0;;Yes x<-[statfile (p@name);;p<-path]]~=[]

and sourcePath = case includePath
		 in []: ["./"]
		 || _: includePath
		 end

#if 1
and libraryPath = getpath libraryPathVar ["./";hbc_library]
and includePath = getpath "LMLINCLUDE" []

and getpath = getenvf (map (@"/") o chop)

#else
--old version:
and libraryPath =
  case getenv libraryPathVar
  in Yes path: map (@"/") (chop path)
  || No _: [hbc_library]
  end

and includePath = lmlIncludePath
and lmlIncludePath =
  case getenv "LMLINCLUDE"
  in Yes ps: map (@"/") (chop ps)
  || No _: []
  end
#endif

and getenvf f var def =
  case getenv var
  in Yes s: f s
  || No _: def
  end

and getenvdef = getenvf id

and hbc_library = lmldir@"hbc_library/"

and lmldir = (getenvdef "LMLDIR" "/usr/local/lib/lmlc")@"/"

and chop = choplist (splitat ':')
and lines = choplist (splitat '\n')
and unindent = dropto (not o isspace)	-- remove leading white space

#if 1
-- There is a space leak in filter that can be avoided by compiling with
-- the -fzap-redex flag. (The standard library is not compiled with this flag.)
and filter p [] = []
 || filter p (x.xs)= if p x then x.filter p xs else filter p xs
#endif

end
