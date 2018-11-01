module
#include "../misc/misc.t"
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../ExprE/Expr_t.t"
#include "../ExprE/Eprint.t"
#include "../ExprE/unrec.t"
#include "lliftutil.t"
#include "../misc/setofid.t"
#include "../misc/util.t"

export lambdalift, lambdalift1;

rec lambdalift (Emodule i exl dss) u =
   let (dss', u') = Umap lift1 dss u
   in (Emodule i exl dss', u')

and lambdalift1 (Emodule i exl dss) u =
    let dss1 = map (mapsnd toplam) dss in
    let (dss', u') = Umap lift1 dss1 u in
    let ds2 = concmap (mapsnd untoplam) dss' in
    (Emodule i exl (map snd (sccds ds2)), u')
and toplam (e as Elaml _ _) = e
----||  toplam (e as Einfo inline (Elaml _ _)) = e
||  toplam e = Elaml [] e
and untoplam (Elaml [] e) = e
||  untoplam e = e

and lift1 dl unique =
   let Xset sol s z = itlist(\f.\p.Iu (assocdefeq eqid f sol []) p) s z 
   in
   let rec 
   	   Le fid (Elet r d e) vars funs sol u =
		let rec (ed,dd,id,dvars,dfuns,ss,ud) = Ld fid d nvars nfuns nsol u
		    and (ee,de,ie,ue) = Le fid e nvars nfuns nsol ud
		    and nvars = Iu vars dvars
	            and nfuns = Iu funs dfuns
		    and nsol = 	if r then -- if recursive let
		    	       	    solve(map(\(f,S).
			        	    f,
					    Xset sol (Is S funs) (Is S nvars),
					    Is S dfuns)
					    ss) @ sol
				else
				    -- map(\(f,S).(f, Is S vars)) ss @ sol
				    map(\(f,S).(f, Xset sol (Is S funs) 
				    		        (Is S vars))) ss @ sol
		in (case ed in []:ee || _._: Elet r ed ee end,
			    dd@de, Iu id ie, ue)
	|| Le fid (Eidapl f E) vars funs sol u =
		let! (eE,dE,iE,uE) = La fid E vars funs sol u
		in (Eidapl f (map idexp (assocdefeq eqid f sol []) @ eE),
		    dE, Iu [f] iE, uE)
	|| Le fid (Elaml I e) vars funs sol u =
	        let! (ee,de,ie,ue) = Le fid e (Iu vars (Imk I)) funs sol (u+1)  in
		let iu = numid u fid in
		let Ifree = Xset sol (Is funs ie) (Is vars ie) in
		   (Eidapl iu (map idexp Ifree), 
		    (iu, Elaml (Ifree @ I) ee).de,
		    ie, ue)

	|| Le fid (Econstr c E) vars funs sol u = 
		let! (eE,dE,iE,uE) = La fid E vars funs sol u
		in (Econstr c eE, dE, iE, uE)
	|| Le fid (Einfo f e) vars funs sol u =
	        let! (ee,de,ie,ue) = Le fid e vars funs sol u
		in (Einfo f ee, de, ie, ue)

	|| Le fid (Ecase e p f) vars funs sol u =
		let rec Lp [] vars funs sol u = ([], [], [], u)
		     || Lp ((c,I,e).p) vars funs sol u =
		        let! (ee,de,ie,ue) = Le fid e (Iu(Imk I)vars) funs sol u in
		        let! (ep,dp,ip,up) = Lp p vars funs sol ue in
			   ((c,I,ee).ep, de@dp, Iu ie ip, up)
		in
	        let! (ee,de,ie,ue) = Le fid e vars funs sol u  in
	        let! (ep,dp,ip,up) = Lp p vars funs sol ue in
	        let! (ef,df,id,uf) = Le fid f vars funs sol up in
		   (Ecase ee ep ef, de@dp@df, Iu (Iu ie ip) id, uf)

	|| Le fid (Efailmatch n) vars fun sol u =
		(Efailmatch n, [], [], u)
	
	|| Le fid (e as Ecfunction _ _) vars fun sol u =
		(e, [], [], u)
	

       and La fid   []  vars funs sol u = ([], [], [], u)
	|| La fid (e.l) vars funs sol u = 
		     let! (ee,de,ie,ue) = Le fid e vars funs sol u  in
		     let! (el,dl,il,ua) = La fid l vars funs sol ue
		     in (ee.el, de@dl, Iu ie il, ua)

       and Ld fid [] vars funs sol u = ([], [], [], [], [], [], u)
	|| Ld fid ((op as (f, (Elaml (I as _._) e))).l) vars funs sol u =
	             let fid' = case I in [] : fid || _ : f end in
		     let! (ee,de,ie,ue) = Le fid' e (Iu vars (Imk I)) funs sol u in
		     let! (el,dl,il,dvars,dfuns,ss,ud) = Ld fid' l vars funs sol ue
		     in (el, deltype op (f,Elaml (assocdefeq eqid f sol [] @ I) ee).de@dl,
		        Iu ie il, dvars, Iu [f] dfuns, (f, ie).ss, ud)
---	|| Ld fid ((op as (f, Einfo inline (Elaml (I as _._) e))).l) vars funs sol u =
---	             let fid' = case I in [] : fid || _ : f end in
---		     let! (ee,de,ie,ue) = Le fid' e (Iu vars (Imk I)) funs sol u in
---		     let! (el,dl,il,dvars,dfuns,ss,ud) = Ld fid' l vars funs sol ue
---		     in (el, deltype op (f,Einfo inline (Elaml (assocdefeq eqid f sol [] @ I) ee)).de@dl,
---		        Iu ie il, dvars, Iu [f] dfuns, (f, ie).ss, ud)
	|| Ld fid ((x,e).l) vars funs sol u =
	       	     let! (ee,de,ie,ue) = Le fid e vars funs sol u  in
		     let! (el,dl,il,dvars,dfuns,ss,ul) = Ld fid l vars funs sol ue
		     in ((x,ee).el, de@dl, Iu ie il, 
		         Iu [x] dvars, dfuns, ss, ul)

    in 
    let rec Ldtop [] u = ([], u)
	 || Ldtop ((f,Elaml I e).l) u =
		     let! (ee,de,ie,ue) = Le f e (Imk I) [] [] u  in
		     let! (el,ul) = Ldtop l ue  in
			((f,Elaml I ee).de@el, ul)
---	 || Ldtop ((f,Einfo inline (Elaml I e)).l) u =
---		     let! (ee,de,ie,ue) = Le f e (Imk I) [] [] u  in
---		     let! (el,ul) = Ldtop l ue  in
---			((f,Einfo inline (Elaml I ee)).de@el, ul)
    in
    let (liftedfundefs, newunique) = Ldtop dl unique
    in (liftedfundefs, newunique)
end
