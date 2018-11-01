module
#include "../misc/flags.t"
#include "../misc/ioc.t"
#include "../misc/misc.t"
#include "../misc/util.t"
#include "../lib/dialog.t"
#include "../expr/read.t"
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../expr/pprint.t"
#include "../expr/ttype_t.t"
#include "../expr/ttype.t"
#include "../expr/types_t.t"
#include "../rename/renenv.t"
#include "../rename/multi.t"
#include "../rename/importlib.t"
#include "../ExprE/predef.t"
#include "../ExprE/classtrans.t"		/* id_callmethod */
#include "cexpr.t"
#include "iload.t"
#include "imisc.t"
#include "iwhatis.t"
#include "icomp.t"
#include "compile.t"
#include "state.t"
#include "idsubst.t"
#include "topprt.t"
#include "mkprt.t"

export topstart, nvalues,nlibs;
rec type Command = 
	Com_eval Texpr + 
	Com_bind Binding + 
        Com_mbind Texpr Texpr +
	Com_load (List (String#(List Impid))) +
	Com_cload (List (String#(List Int))) Import +
	Com_mload String Texpr +
	Com_end + 
	Com_empty +
	Com_msg String +
	Com_msgnp String +
	Com_whatis String +
	Com_help +
	Com_show_ String
and readfile s = 
	case openfile s in
	   No e : fail (e@" "@s)
	|| Yes s : s
	end
and h13ext = if H1_3 then "1.3/" else "/"
and lib          = lmldir @ "/lib/"
and hlib         = lmldir @ "/hlib" @h13ext
and hbc_library  = lmldir @ "/hbc_library" @ h13ext
and bigopname = "Bigops"
and showname = "Ishow"
and helpfile = (if Curry then hlib else lib)@"ihelp"
and     readni l =
	let (ll, s) = readstring l in
	let (lll, imps) = readimplist ll in
	(lll, (s, imps))
and     readograph l =
	let (ll, s) = readstring l in
	let (lll, deps) = readlist readstring ll in
	(lll, (s, map stoi deps))
and     topparse "" = (Com_end, "")
||      topparse (' '.l) = topparse l
||      topparse ('^'.l) = (Com_empty, l)
||      topparse ('H'.l) = (Com_help, l)
||      topparse ('|'.l) =
        let (ll, nis) = readlist readni l in
	(Com_load nis, ll)
||      topparse ('c'.l) =
	let (ll, (m0,_).ssr) = readlist readograph l in
	let (lll, imp) = readimport ll in
	let ss = (m0,[2..length ssr+1]).ssr in
	(Com_cload ss imp, lll)
||	topparse ('@'.l) =
        let (ll, s) = readstring l in
	let (lll, e) = readexpr ll in
	(Com_mload s e, lll)
#if 0
||	topparse ('o'.l) =
	let (ll, e) = readlist readstring l in
	(Com_oload e, ll)
#endif
||      topparse ('{'.l) =
	let (ll, e) = readexpr l in
	(Com_eval e, ll)
||      topparse ('M'.l) =
	let (ll, p) = readexpr l in
	let (lll, e) = readexpr ll in
	(Com_mbind p e, lll)
||      topparse ('}'.l) =
	let (ll, b) = readbinding l
	in (Com_bind b, ll)
||	topparse ('`'.l) =
	let (ll, mkconst (cstring s)) = readexpr l in
	(Com_msg s, ll)
||	topparse ('/'.l) =
	let (ll, mkconst (cstring s)) = readexpr l in
	(Com_msgnp s, ll)
||	topparse ('?'.l) =
	let (ll, s) = readstring l in
	(Com_whatis s, ll)
||	topparse ('_'.l) =
	let (ll, s) = readstring l in
	(Com_show_ s, ll)
||	topparse s = ifail ("Bad input!! '" @ s @ "'\n")

-----
and ntoploop state cmds = 
    sigact excError (SACatch (\e.
       ("Internal interpreter error: "@e@"\n")@@toploop state (tl cmds))) $
    sigact excInterrupt SAIgnore $
    ntoploop1 state cmds
--trace (show_Renv (st_env state))
and ntoploop1 state (cmd.cmds) =
	case cmd in
	   Com_end : "Bye\n"@@
#ifdef sparc
			    failexit 0	-- failexit is ugly, but avoids a bug in the catch/throw handling on SPARC
#else
                            done
#endif
	|| Com_empty : toploop state cmds
	|| Com_eval e :
		case cexpr state e in
		   Yes E :
			let e' = idsubst (st_lib state) E in
			let v = compile e' in \c.(if TopShow then ppr e@"\n"@show_Cexpr e' @ "\n" else "") @@ topprt v c
		|| No msg : (msg @@)
		end $ "\n" @@ toploop state cmds
	|| Com_load siss :
                let (ss', iss) = split siss in
		let ss = map add_o ss' in
	        concmap loadingmsg ss @@
		case cload state ss (conc iss) in
		   Yes (ids, state') : toploop (addminfo ss ids state') cmds
		|| No msg : msg @@ "\n" @@ toploop state cmds
		end
	|| Com_cload ssg' i : 
	   if ReallyLoadShareLib then
	    mapc findload (map fst ssg') $ \lfiles.
	    if not (null [s;; No s <- lfiles]) then
		"Some modules cannot be loaded\n"@@toploop state cmds
	    else let
	    rec ssg0 = combine (lfiles, (map snd ssg'))
	    and ssg = mapfst (\(Yes s).s) ssg0
	    and nss = combine (mappair (number 1) 
			       (map (map (\i.(i,fst (select i ssg)))))
				   (split ssg))
	    and ssl = map (map fst) (scceq (\x.\y.fst x=fst y) nss)
	    and (s0l, mals) = mappair (map snd) 
		                   (remdups (st_lib state) o mkset o map snd) 
		                (partition ((=1) o fst) (conc ssl))
	    and (s0.als) = mkset (s0l @ mals)
	    and cycles = filter ((>1) o length o mkset) (map (map snd) ssl)
	    in (if ~ null cycles 
		then "Warning: cyclic dependencies:\n"@ 
		    concmap (@"\n") (map (\c.mix c " ") cycles)
		else "")@@
	       loadingmsg s0 @@
	       case ccload state (als@[s0]) i in
		   Yes (ids, state') : 	       
		       (concmap (\s."Autoloading \""@s@"\"\n") als)@@
		       toploop (addminfo [fst (hd ssg')] ids state') cmds
		|| No msg : msg @@ "\n" @@ toploop state cmds
		end
	   else
	       let ss1 = map fst ssg' in
	       let ss = hd ss1 . filter (shouldauto (st_lds state)) (tl ss1) in
                loadingmsg (hd ss) @@
                case ccload state ss i in
                   Yes (ids, state') :         
                       concmap (\s."Autoloading \""@s@"\"\n") (tl ss)@@
		       toploop (addminfo [fst (hd ssg')] ids (addlds ss state')) cmds
		|| No msg : msg @@ "\n" @@ toploop state cmds
                end

	|| Com_mload s e :
	        loadingmsg s @@
		case cmodule state e in
		   Yes (ids, state') : toploop (addminfo [s] ids state') cmds
		|| No msg : msg @@ "\n" @@ toploop state cmds
		end
#if 0
        || Com_oload os :
	       concmap (\s."Autoloading \""@s@"\"\n") os@@
	       case dloadmodules os in
	          No msg : msg @@ ntoploop state cmds
	       || Yes ies : ntoploop state cmds
               end
#endif
	|| Com_bind b :
		case cbind state b in
		   Yes (ids, state', ies) : 
			let r = prenv ids @@ toploop state' cmds in
			if TopShow then
			    show_list (show_pair(prid,show_Cexpr)) ies @@ "\n" @@ r
			else
			    r
		|| No msg : msg @@ "\n" @@ toploop state cmds
		end
	|| Com_mbind p e :
	    let b = mkbpat [(p, mkap (mkident (mkids "__unsafePerformIO")) e)]
	    in	case cbind state b in
		   Yes (ids, state', ies) : 
--- !!! XXX really ugly.
	                let xcmd = Com_eval (mkap (mkap (mkident (mkids "_seq")) (if null ies then p else mkident (mkids (idtostr (fst (hd ies)))))) (mkap (mkident (mkids "_putStr")) (mkconst (cstring "")))) in
			let r = prenv ids @@ ntoploop1 state' (xcmd.cmds) in
			if TopShow then
			    show_list (show_pair(prid,show_Cexpr)) ies @@ "\n" @@ r
			else
			    r
		|| No msg : msg @@ "\n" @@ toploop state cmds
		end
	|| Com_msg s : s @@ toploop state cmds
	|| Com_msgnp s : s @@ ntoploop state cmds
	|| Com_whatis i : whatis state i @@ toploop state cmds
        || Com_show_ i :
	        case mkprt state i in
		   Yes b : ntoploop state (Com_bind b . cmds)
		|| No msg : msg @@ "\n" @@ toploop state cmds
		end
	|| Com_help : 
	       case openfile helpfile in
		   No _ : "Sorry, cannot open help file\n"
	       ||  Yes msg : msg
	       end @@ toploop state cmds
	end
and toploop state cmds = "> " @@ ntoploop state cmds
and topstart i = 
    let r = toploop (addshow (lib@showname) (st_mk i_ipreenv startlib i_unum1 ideflts [] [])) (choplist topparse i @ [Com_end]) in
    if TopShow /**& ImpDebug**/ then
	("startlib: "@show_list prsymbol startlib) @@"\n"@@
	("preenv: "@show_Renv i_ipreenv) @@"\n"@@
	r
    else
	r
and loadingmsg s = "Loading \""@s@"\"\n"
and ideflts = [Tinteger; Tdfloat]
and bigenv = rlist Kvalue (bigeqops@bigordops)
and selenv = rlist Kvalue selids
and metenv = rlist Kvalue (ivectorwrap.for 2 15 id_callmethod)
and allenv = rjoin (rjoin (rjoin metenv selenv) bigenv) i_ipreenv
and loadone f = 
    case dloadmodule f in
	No msg : fail msg
    ||  Yes ies : dconvies allenv ies
    end

and nlibs = length [x;; DynLib x y <- startlib]
and nvalues = length [x;; Internal x y <- startlib]

and addminfo (s._) ids = st_map_menv ((baseName s,ids).)
and startlib =
    let builtins = if ReallyLoadShareLib then []
		   else loadone "" in	        -- to get the predefined stuff
    (if Curry & length builtins >= 0 then	-- length just to force evaluation (strange side effects in loadmodule!)
	 Internal ivectorwrap (Comb "Pvectorwrap" (fail "ivectorwrap")) .
	 (if ReallyLoadShareLib then
	      loadone (lmldir@"/lib/lib_i.so") @
	      loadone (hlib@"/lib_i.so") @
	      loadone (hbc_library@"/lib.so")
	  else loadone (hlib@"/Hprel.o")
	      ) @ loadone (lib @ add_o bigopname)
    else if ReallyLoadShareLib then loadone (lmldir@"/lib/lib_i.so")
	                            
    else [])@ 
    -- Do a first call to loadmodule just to get the initial table of identifiers.
    builtins

and shouldauto lds f = ~H1_3 | ((head nhlib f ~= hlib | has__ f) & ~mem f lds
	where rec nhlib = length hlib
        and       has__ ('_'.'_'._) = true
        ||        has__ (_.cs) = has__ cs
        ||        has__ "" = false
        )

and addshow f state = if Curry then state else
	case cload state [f@doto] (snd (readimplist (readfile (f@".p")))) in
	   Yes (_, state') : state'
	|| No msg : fail msg
	end
and addext s e = stripextension s @ e
and add_o s = addext s doto
and doto = if ReallyLoadShareLib then ".so" else ".o"
and isnone None = true
||  isnone _    = false
and findload n c = 
    let ofile = addext n ".o"
    and sofile = addext n ".so" 
    and bs n s = rev (snd (splitat '/' (rev n))) @ '/'.s
    and trywith s e = if file_exists s then c (Yes s) else e
    in trywith (bs n "lib_i.so") $ trywith (bs n "lib.so") $
       if file_exists ofile | file_exists sofile then
	   if older sofile ofile then buildso n ofile sofile c
	   else c (Yes sofile)
       else "Cannot find object file for "@@n@@"\n" @@ c (No n)

and buildso n ofile sofile c = 
    let buildcmd = lmldir@"/bin/buildshlib \"\" "@sofile@" "@ofile
    in ("Making "@sofile@" from "@ofile@"\n")@@
       system buildcmd (\_. ("Cannot build "@@sofile@@"\n"@@ c (No n))) $
       c (Yes sofile)

and file_exists = not o isno o openfile

and filemodtime filename =
  case statfile filename
  in Yes stat: Some (select 10 stat)
  || No _ : None
  end

and older f1 f2 = case filemodtime f1 in
		     None: true
		  || Some t1: case filemodtime f2 in
				 None: false
			      || Some t2: t1 < t2
			      end
		  end

and mapc cmd [] c     = c []
||  mapc cmd (x.xs) c = cmd x $ \r. mapc cmd xs (c o (r.))

/* not possible to reload a module now ... */			   
and remdups lib ss = 
    mapfilter (\s.if null [s';;DynLib s' x <- lib; s' = s] 
		  then Some s else None) ss
end

#if 0
and readdir s = case opendir s in Yes ss : ss || No msg : fail msg end
	case dloadmodules (map (\s.trace s ("hlib/"@s)) (readdir "hlib")) in
	    No msg : fail msg
        ||  Yes ies : dconvies ipreenv ies
        end
#endif
