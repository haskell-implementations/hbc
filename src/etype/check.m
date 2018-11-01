module -- check
--
-- $Header: /ufs/u10.cs/augustss/src/mtcroot/LML/etype/check.m,v 1.8 1996/04/03 22:31:59 augustss Exp $
--
-- performs the actual type checking
--
#include "../expr/constr_t.t"
#include "../expr/einfo_t.t"
#include "../expr/ttype_t.t"
#include "../expr/ttype.t"
#include "../expr/types_t.t"
#include "../expr/eq.t"
#include "../expr/id.t"
#include "../expr/id_t.t"
#include "../expr/pprint.t"
#include "../expr/constrfun.t"
#include "../expr/exprid.t"
#include "../transform/misc.t"
#include "../type/subst_t.t"
#include "../type/prefix.t"
#include "../type/subst.t"
#include "../misc/misc.t"
#include "xsubst.t"
#include "../type/unify.t"
#include "earley.t"

export Wb,W,Wp,prposs;

rec prcaseposs eTtts =
     conc (map2 (\(e,T,t1,t2).\n. 
                    "caseNr: "@show_int n@"\nExp: "@concmap ppr e@
                                    "Subst: "@ prTR T@
                                    "\nPat-type: "@prttype t1@
                                    "\nExp-type: "@prttype t2@"\n\n"
                )
                eTtts
                (from 1)
          )

and prposs eTts = 
      conc (map2 (\(e,T,t).\n. 
                    "Nr: "@show_int n@"\nExp: "@concmap ppr e@
                                    "\nSubst: "@ prTR T@
                                    "\nType: "@prttype t@
                                    "\n\n"
                 )
                 eTts
                 (from 1)
           )

/* ecombine : List *a -> List *b -> List (*a # *b)
   ecombine [x1,x2,...xn] [y1,y2,...,ym] = [(x1,y1),(x1,y2),...,(x1,ym),
                                            (x2,y1),(x2,y2),...,(x2,ym),
                                             .
                                             .
                                            (xn,y1),(xn,y2),...,(xn,ym)]
*/         
and ecombine [] _ = []
||  ecombine (x.xs) ys = (map (\y.(x,y)) ys) @ ecombine xs ys

-- combs : List (List *a) -> List (List *a)
and combs []       = [[]]
||  combs (xs.xss) = let yss = combs xss
                     in concmap (\x.map (\ys. x.ys) yss) xs

-- remove dublets from a list of Texpr
and removedubl [] = [] 
||  removedubl (e.l) = if member eqTexpr e l
                       then removedubl l
                       else e.removedubl l

-- rearrange [[e1,e2,...,en],[e1',e2',...,en'],...,[e1'm,e2'm,...,en'm]] = 
--     [[e1,e1',...,e1'm],[e2,e2,...,e2'm],...,[en,en',...en'm]]
and rearrange [] = []
||  rearrange (L as [].ll) = []
||  rearrange ll = -- (map hd ll).(rearrange (map tl ll))
                      (removedubl (map hd ll)).(rearrange (map tl ll))

and mkvbsub = reduce2 (\(_,mktvar u).\(_,t).\S. addTR (u,t) S) emptyTR

/* Dessa anv{nds inte nu 
and
    echk f (bad [s]) = bad [' '.ppr f ; s]  
||  echk _ t       = t

and ecombTRs f Ss   = echk f (combTRs Ss)
and ecombTR f s1 s2 = echk f (combTR s1 s2)
and cst ((c, _ ,_)._) = ctype c
and consinst c u = instTR (getTvars (cst c)) u
*/



--  Wb : Prefix -> Binding -> Int -> (List Texpr # Vbind # Subst # Int)
and Wb p f u = case Wbi p f u 
               in ([(es,vb,S)],u2) :  (es,vb,S,u2)    -- entydigt OK!!
               || ([],_)      : fail ("Cannot type (in Wb):\n" @
                                       (prdefg 0 f))
               || (esvbSs,_)  :               -- flertydigt
                       fail ("Ambiguity!!:\n"   @
			        (concmap (\es.concmap ppr es @ "\n")
				         (rearrange (map fstof3 esvbSs))
			        )
			    ) 
               end

and Wbi p f u  =
--  Wbi : Prefix -> Binding -> Int -> (List (List Texpr # Vbind # Subst) # Int)
	case f 
	in mkbtype t ats None _ :
           	([([],[], emptyTR)], u) 

	|| mkbctype t cgss	:
           	([([],[], emptyTR)], u) 
        
--         || mkbpat [(mkident i ,mkinfo (restr _ t) e)] : G}r ej s} l{tt!
                

	|| mkbpat [(mkident i, e)] :
            let t1 = mktvar u in 
            let (esSts,u1) = W p e t1 (u+1) in 
              (map (\(es,S,t).(es,[(i,TRtype S t1)],pruneTR u S)) esSts, u1)

	|| mkbmulti pt e	:
            let t1 = mktvar u in 
              case Wp p pt t1 (u+1) 
              in ([(es1,vb,R,_)],u1) :
                     let (esSts,u2) = W p e (TRtype R t1) u1 in
                      (concmap (\(es2,S,_).
			           case combTR R S 
				   in bad _ : []
				   || V : [(es1@es2, TRvbind V vb, V)]
      				   end
                               )
                               esSts
                         , u2
                      )
              || (esvbRrs,u1)        : 
                     let (esSts,u2) = W p e t1 u1 in 
                       (concmap (\((es1,vb,R,_),(es2,S,_)).
                                      case combTR S R
                                      in bad _ : []
                                      || V     : [(es1@es2,TRvbind V vb,V)]
                                      end 
                                )
                                (ecombine esvbRrs esSts)
                          , u2
                       )
              end

	|| mkband b1 b2		:
		let (esvbRs,u1) = Wbi p b1 u in
		let (esvbSs,u2) = Wbi p b2 u1 in
                  (concmap (\((es,vb,R),(es2,vb2,S)).
                                case combTR S R 
                                in bad _ : []
                                || V     : [(es@es2,TRvbind V (vb@vb2),V)]
                                end
                           )
                           (ecombine esvbRs esvbSs)
                     , u2
                  )

	|| mkbrec b	:
		let ib = idbind b in
		let u1 = u + length ib in
		let ts = for u (u1 - 1) mktvar in
		let px = addngs (combine (ib,ts)) p in
		let (esvbRs,u2) = Wbi px b u1 in
                  (concmap (\(es,vb,R).
          		      let Ss = map2 Unify (map snd vb) ts in
                                case combTRs (R.Ss) 
                                in bad _ : []
                                || V     : [(es,TRvbind V vb,V)]
                                end
                           )
                           esvbRs
                    , u2
                  )

	|| mkblocal b1 b2	:
		let (esvbRs,u1) = Wbi p b1 u in
                let (u3,esvbVs) =
                      mapstate (\u.\(es,vb,R).
                                 let (esvbSs,u2) = Wbi (addpre vb p) b2 u1 in
                                   (u2,concmap (\(es2,vb2,S).
                                                 case combTR S R 
                                                 in bad _ : []
                                                 || V     : [(es@es2,TRvbind V vb2,V)]
                                                 end
                                               )
                                               esvbSs
                                   )
                               )
                               u1
                               esvbRs
                in 
                    (conc esvbVs,u3)

	|| mkbnull	:
           	([([], [], emptyTR)], u)
	|| mkbpragma _	:
           	([([], [], emptyTR)], u)
	end

and Wp p f et u = case Wpi p f et u 
               in ([],_) : fail ("Cannot find a type for " @ ppr f)
               || other  : other
               end
and Wpi p f et u =
--  Wpi : Prefix -> Texpr (repr. pattern) -> Ttype -> Int -> 
--	 (List (List Texpr # Vbind # Subst # Ttype) # Int)
        case f
        in mkident i	:
                ([([],[(i,et)], emptyTR, et)], u)

	|| mkas i pt 	: 
	      	let (esvbRrs,u1) = Wp p pt et u in
	      	(map (\(es,vb,R,r).(es,(i,r).vb,pruneTR u R,r)) esvbRrs , u1)

/* e typcheckas flera ggr. Skriv om som caseuttrycket!
       	|| mkcondp pt e	:
	      	let (esvbRts,u1) = Wp p pt u in
                let (u3,esvbSts) = 
                   mapstate (\u.\(es,vb,R,t).
                   	      let (esSts,u2) = W (addngs vb p) e u1 in
                               (u2,concmap (\(es2,S,t2). 
	                	             let U = Unify Tbool t2 in
	      				      case combTRs [U;S;R] in
                                                bad _ : []
                                              || V    : [(es@es2,TRvbind V vb,V,TRtype V t)]
                                              end
                                           )
                                           esSts
                               )
                            )
                            u1
                            esvbRts
                in
                   (conc esvbSts,u3)
*/
	|| mkconstr (Cconstr _ ctyp _ _ _ ts) es :   
		let (T,u1) = instTR (getTvars ctyp) u in
                let ntyp = T ctyp in
                let U = Unify et ntyp in
		let (u2,esvbSss) =
		    mapstate (\u.\(t,e).
                       let (esvbRrs,u3) = Wp p e (TRtype U (T t)) u in
			      (u3, map (\(eseq,vb,R,r).(eseq,vb,R))
                                       esvbRrs
                              )
                             )
                             u1
			     (combine (map fst ts, es))             
		in 
                  (concmap (\esvbSs.
                             let (ess,vbs,Ss) = split3 esvbSs in
                              case combTRs (U.Ss)
                              in bad _ : []
                              || V  : [(conc ess,
                                        TRvbind V (conc vbs),
                                        pruneTR u V,
                                        TRtype V ntyp
                                       )]
                              end
                           )
                           (combs esvbSss)
                    ,u2
                  )
 
	|| mkbrack g lexs      : 
            case earley true p g et lexs u 
            in ([],u1) : fail ("Cannot parse : " @ ppr f 
                                    @ "using grammar " @ prgram g
/*
			            mix (map (pralts 0) g) ("\n and ")
			 	      where   pralts i (tt, altlist) =
			                        prttype tt @ " ::= " @
			                        mix (map ((prprod i) o snd) altlist) " + "
*/
                                  )
            || (evbSts,u1) : (map (\(e,vb,S,t).([e],vb,pruneTR u S,t)) evbSts , u1)
            end

	|| mkinfo i pt : 
	    case i 
	    in restr _ t : 
                let S = Unify t et in 
                let (esvbRrs,u1) = Wp p pt (TRtype S t) u in 
                   (concmap (\(es,vb,R,r).
                                case combTR S R 
                                in bad _ : []
                                || V : [(es,TRvbind V vb,pruneTR u V,TRtype V t)] 
                                end
                             )
                             esvbRrs
			   , u1
                   )
	    ||   _ : Wp p pt et u 
            end

	|| mkconst _	: fail "mkconst should not appear during typechecking!"
	|| mkap _ _	: fail "mkap should not appear in patterns!"
	|| mkcfunction _ _ : fail "XXX cfunction"
	end

and W p f et u = case Wi p f et u 
                 in ([],_) : fail ("Cannot find a type for " @ ppr f)
                 || eStsu  : /*trace ("Expression: " @ ppr f @ 
     			            "\nExpected type: " @ prttype et @ 
  				    "\nFound parsed expressions,
substitutions and types: " 
			          @ (concmap (\(es,S,t). prttype t @ " " @ prTR
S @ "\n" @ concmap ppr es @"\n\n")
                                             (fst eStsu) 
			            )
                                   ) */ 
                                   eStsu
                 end 
and Wi p f et u =
--  Wi : Prefix -> Texpr -> Ttype -> Int -> (List (List Texpr # Subst # Ttype) # Int)
	case f
	in 
           mkap d e :
            let t1 = mktvar u in
              case W p e t1 (u+1)  
              in ([(ee,S,t)],u1) 
                    : let (edRrs,u2) = W p d (TRtype S (Tarr t1 et)) u1 
                      in 
                        (concmap (\(ed,R,r).
                                    case combTR R S
       			            in bad _ : []
				    || V    : [(ed@ee,pruneTR u V,TRtype V et)]
				    end
                                 )
				 edRrs
                           ,u2
                        )
              || (eeSts,u2) 
                    : let (edRrs,u3) = W p d (Tarr t1 et) u2 in
    			 (concmap (\((ee,S,t),(ed,R,r)). 
                                     case combTR R S
                                     in bad _ : []
				     || V   : [(ed@ee,pruneTR u V,TRtype V et)]
				     end
                                  )
				  (ecombine eeSts edRrs) 
                             ,u3
                         )
              end


 || mklam (mkident i) d :  
      let t1 = mktvar u in 
      let t2 = mktvar (u+1) in 
      let U = Unify et (Tarr t1 t2) in 
-- trace ("Abstraction-trace: Expected type: " @ prttype et @ "\nSubst:"@prTR U@"\n")
   (
      let px = addngs [(i,TRtype U t1)] p in
      let (esSts,u1) = W px d (TRtype U t2) (u+2) in
                (concmap (\(es,S,t).case combTR U S 
    			            in bad _ : [] 
				    || V     : [(es,pruneTR u V,TRtype V et)]
				    end
                         ) 
                         esSts
                 , u1
                )

)
 || mkcase ce pes : -- trace ("Expected type for case: "@prttype et@"\n") 
   (
     let mkneweTtts [(e,T,tp,te)] u = ([(e,T)],tp,te,T,u)
      || mkneweTtts eTtts u = let pnt = mktvar u in 
         		      let ent = mktvar (u+1) in 
                                (map (\(e,T,tp,te).(e,addTRs [(u,tp);(u+1,te)] T))         
				     eTtts
                                  ,pnt,ent,emptyTR,u+2
				) 
     in  
     let mknewvbsub [(ep,vb,R,r)] u = ([(ep,R,r)],vb,u)
      || mknewvbsub epvbRrs u =
            let nvb = map2 (\(x,t).\u.(x,mktvar u))      
  		           (sndof4 (hd epvbRrs))   -- the bound id:s
	   	           (from u)
            in
              (map (\(ep,vb,R,r).(ep,combTR (mkvbsub nvb vb) R,r)) epvbRrs
                , nvb , u+ length nvb
              )
     in
     let t1 = mktvar u in 
     let (ceTts,u1) = W p ce t1 (u+1) in
     let (u2,p,eVtts) = -- trace (prposs ceTts) 
     (
       revitlist (\(ep,e).\(u3,p,eTtts). 
                   let (eTs,tp,te,U,u4) = mkneweTtts eTtts u3 in 
                   let p = TRprefix U p  in
                   let (epvbRrs,u5) = Wp p ep tp u4 in
                   let (epRrs,nvb,u6) = mknewvbsub epvbRrs u5 in
		   let (eeSss,u7) = W (addngs nvb p) e te u6 in
                   let (u7,p,eTtts) =    
		      (u7,p,concmap (\((es,T),((ep,R,r),(ee,S,s))).    
				      case combTRs [T;R;S] 
				      in bad _ : []
				      ||  V    : [(es@ep@ee,V,TRtype V tp,TRtype V te)]
				      end
   				  )
			          (ecombine eTs (ecombine epRrs eeSss))
                      )
                   in  -- trace (prcaseposs eTtts)
                            (u7,p,eTtts) -- result 
                 )
                 pes
                 (u1,p,map (\(ce,T,_).(ce,T,TRtype T t1, TRtype T et)) ceTts)
          )
       in
         (map (\(es,V,pt,et).(es,pruneTR u V,et)) eVtts,u2)
       )		

        || mkletv b e : 
            let (es1,vb,R,u1) = Wb p b u in
            let px = addpre (TRvbind R vb) p in
            let (esSts,u2) = W px e (TRtype R et) u1 in 
             (concmap (\(es,S,t).case combTR R S 
                                 in bad _ : []
                                 || V     : [(es1@es,pruneTR u V,TRtype V et)]
                                 end
                       )
                       esSts
                 , u2
              )

	|| mkident i :          
		let (t,gl) = pfind i p in
		let (t,u1) = inst t gl u in
                let V      = Unify t et in 
--     trace (prid i @ " " @ prttype (TRtype V t)) 
           (
		([([],V ,TRtype V t)], u1)
           )

	|| mkconstr (Cconstr _ ctyp _ _ _ ts) es :   
		let (T,u1) = instTR (getTvars ctyp) u in
                let ntyp = T ctyp in 
                let U = Unify et ntyp in 
		let (u2,esSss) =
		    mapstate (\u.\(t,e).
                       let (esRrs,u3) = W p e (TRtype U (T t)) u in
			  (u3, map (\(eseq,R,r).(eseq,R))
                                   esRrs
                          )
                             )
                             u1
			     (combine (map fst ts, es))             
		in 
                  (concmap (\esSs.
                             let (ess,Ss) = split esSs in
                              case combTRs (U.Ss)
                              in bad _ : []
                              || V  : [(conc ess,pruneTR u V,TRtype V ntyp)]
                              end
                           )
                           (combs esSss)
                    ,u2
                  )

	|| mkinfo i e : 
	    case i 
	    in restr _ t : 
                 let S = Unify t et in 
                 let (esRrs,u1) = W p e (TRtype S t) u in 
                    (concmap (\(es,R,r).
                                  case combTR R S 
                                  in bad _ : []
                                  || V     : [(es,pruneTR u V,TRtype V t)] --ok
                                  end
                              )
                              esRrs
			 , u1
                    )
	    ||   _ : W p e et u 
            end

	|| mkbrack g lexs      : 
              case earley false p g et lexs u 
              in ([],u) : fail ("Cannot parse : " @ ppr f 
                                    @ "using grammar " @ prgram g
/*
			            mix (map (pralts 0) g) ("\n and ")
			 	      where   pralts i (tt, altlist) =
			                        prttype tt @ " ::= " @
			                        mix (map ((prprod i) o
snd) altlist) " + "
*/
                               )
              || (esvbSts,u1) :
                     (map (\(e,vb,S,t).([e],pruneTR u S,t)) esvbSts , u1)
              end

	|| mkmodule _ _ _ _ _	: fail "mkmodule during typechecking?" 
	|| mkconst _  		: fail "mkconst during typechecking?"
	|| mkerror err 		: fail err
        || mkas _ _    		: fail "mkas only in patterns"
	|| mkcondp _ _  	: fail "mkcondp only in patterns"
	|| mkfailmatch _ 	: fail "mkfailmatch during typechecking?"
	|| mkcfunction _ _      : fail "XXX cfunction"
	end

end
