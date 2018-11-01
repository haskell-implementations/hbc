module
#include "../expr/einfo_t.t"
#include "../expr/id.t"
#include "../expr/id_t.t"
#include "../ExprE/Expr_t.t"
#include "../misc/flags.t"
#include "lliftutil.t"

export addclose;
rec
    addclose (Emodule i expl dl) =

     let rec 
            badframe i = id_visibility i = Vimported & framesize_of_id i < 0
        and 
	    R (Ecase e pl dp) = 
   		Ecase (E e) (map(\(c,il,e).(c, il, R e)) pl) (R dp)
	 || R (Elet r dl e) = Elet r (Cdl dl) (R e)
	 || R (Eidapl i el) =
		if strictprimitive i then
			Eidapl i (map E el)
		else
			Eidapl i (map C el)
	 || R (Einfo strict e) = Einfo strict (R e)
	 || R (Einfo (doeval is) e) = Einfo (doeval is) (R e)
	 || R e = E e
	and E (Ecase e pl dp) = 
   		Ecase (E e) (map(\(c,il,e).(c, il, E e)) pl) (E dp)
	 || E (Elet r dl e) = Elet r (Cdl dl) (E e)
	 || E (Econstr c el) = Econstr c (map C el)
	 || E (Efailmatch n) = Efailmatch n
	 || E (e as Ecfunction _ _) = e
	 || E (e as Eidapl i el) =
		if strictprimitive i then
			Eidapl i (map E el)
		else
		    if nuflag then
			let a = arity_of_id i in
			if a ~= -1 & a < length el & length el ~= 0 | badframe i then
				wrap e
			else
				Eidapl i (map C el)
		    else
			Eidapl i (map C el)
#if 1
--*** Lambda lifting has already been done
 	 || E (Elaml il e) = Elaml il (R e)
#endif
	 || E (e as Einfo noeval _) = e
	 || E (Einfo i e) = Einfo i (E e)

	and C (Eidapl i []) = Eidapl i []
	||  C (e as Eidapl i el) =
   		if strictprimitive i /*| id_ismethod i*/ then
		    wrap (Eidapl i (map E el))
		else
		    if nuflag then
			let a = arity_of_id i in
			if a = -1 | a < length el | badframe i then
			    wrap e
			else
			    Eidapl i (map C el)
		    else
			    Eidapl i (map C el)
#if 1
--*** Lambda lifting has already been done
	 || C (Elaml il e) = Elaml il (R e)
#endif
	 || C (Econstr c el) = Econstr c (map C el)
	 || C (e as Einfo noeval _) = e
	 || C (e as Ecfunction _ _) = e
	 || C (Einfo strict e) = Einfo strict (E e)
	 -- If all definitions are functions they will all be lifted, hence no need to wrap.
#if 0
*** Lambda lifting has already been done
	 || C (Elet re dl e) & (all (isfunc o snd) dl) = Elet re (Cdl dl) (C e)
#endif
         || C (Einfo ii e) & (nowrap ii) = Einfo ii (C e)
	 || C e = wrap e

	and wrap e = Elaml [] (R e)

	and nowrap _ = false
	-- more ???

#if 0
*** Lambda lifting has already been done
	and isfunc (Elaml _ _) = true
	||  isfunc _ = false
#endif

	and Cdl dl =
		let f (i, Elaml il e) = (i, Elaml il (R e))
		 || f (i, e)          = (i, C e)
		in map f dl

	in
	let Cdltop dl =
		let f (i, Elaml il e) = (i, Elaml il (R e))
		 || f (i, e)          = (i, Elaml [] (R e))
		in map f dl
	in
	    Emodule i expl (map Cdltop dl)
end
