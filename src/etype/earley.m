--  5 dec  1991
-- Substitutionen appliceras p} itemet och itemen dekoreras med nummer
-- dekoreringen med precedencer till icketerminalen efter punkten {r borta 
-- 
-- 18 feb  1992
-- Nya typvariabler vid predict ist{llet f|r complete

--  28 juli 1992 
-- Nya typvariabler {ven vid complete
-- Substitutioner fr}n complete och antiquotescan appliceras inte p} 
--     v{nstersidans typ.
-- Lite bugr{ttningar vid predict-- 

--  21 april 1993
-- Ny lexikalanalys, Fördefinierade typer Ident och Number

--  24 maj 1993 
-- Förbättrad lexikalanalys, symboltokens uppdelade efter grammatiktokens

module
#include "../expr/id_t.t"
#include "../expr/constr_t.t"
#include "../expr/einfo_t.t"
#include "../expr/ttype_t.t"
#include "../expr/ttype.t"
#include "../expr/types_t.t"
#include "../expr/eq.t"
#include "../expr/id.t"
#include "../expr/pprint.t"
#include "../expr/constrfun.t"
#include "../expr/tinfo.t"
#include "../transform/misc.t"
#include "../transform/hexpr.t"
#include "../type/subst_t.t"
#include "../type/prefix.t"
#include "../type/subst.t"
#include "../misc/misc.t"
#include "../transform/cutil.t"
#include "xsubst.t"
#include "../type/unify.t"
#include "substear.t"
#include "check.t"

export  earley;

-------------------------------------------------------------------------------

rec type Item =
  ITEM Int             -- Label on this item
       (List Int)      -- Predicted items = are allowed to fetch this item 
                       --                   at complete 
       Ttype           -- Left-hand-side nonterminal 
       (Int # Asstype) -- precedence for the production in this item
       Constr          -- constructor that should be applied to 
                       -- the expression when it is parsed
       Bool            -- true if the production should be forgotten
       (List (Ttype # (Int # Asstype)))  -- left inherited precedences
       (List (Ttype # (Int # Asstype)))  -- right inherited precedences
       (List Texpr)                      -- expressions before the dot
       (List Cgs)                        -- lexical symbols after the dot
       (List (Id # Ttype))		 -- variable binding (only used for
                                         -- patterns, [] otherwise)
       Subst                             -- type substitution
       (List (Ttype # Int))              -- left precedence weights
       (List (Ttype # Int))              -- right precedence weights
       Int                               -- backward pointer
    
-------------------------------------------------------------------------------
/* some trace functions */  
and prItem (ITEM q qL t (p,ass) c b lps rps es cgs vb s lws rws i) = 
      show_int q @ " (" @ concmap show_int qL @ ") " @ 
      prttype t @ "->" @ concmap ppr es @ "." @ concmap (prcgs 0) cgs
@ "   ," @ prvb vb @ "   ," @ prTR s @ " , " @ show_int i @ "\n"

and prUsed used = concmap (\(t,lps,rps,preds).
                              prttype t @"("@prps lps@","@prps rps@") , "
                          )
                          used 

and prEpsi epsi =  concmap (\(q,t,_,_,_,_).
       show_int q @ prttype t @" , ") epsi

and type Token
      = mkterm  Char                                       -- terminal 
      + mktermint Int
      + mktermid String
      + mktermreserved String
      + mktermsym String
      + mktyped Texpr  (List ((List Texpr) # (List (Id # Ttype)) # Subst # Ttype))
                             -- typed expression with possibilities 

and prToken (mkterm c) = [c]
||  prToken (mktermint n) = show_int n
||  prToken (mktermid id) = id
||  prToken (mktermreserved id) = id
||  prToken (mktermsym s) = s
||  prToken (mktyped t p) = " typed "

and isgramterminal (mkct _)    = true
||  isgramterminal (mkctint _) = true
||  isgramterminal (mkctid _)  = true
||  isgramterminal (mkctsym _) = true
||  isgramterminal _           = false

and isterminal (mkterm _)         = true
||  isterminal (mktermint _)      = true
||  isterminal (mktermid _)       = true
||  isterminal (mktermreserved _) = true      
||  isterminal (mktermsym _)      = true
||  isterminal _                  = false

and isNumber (mktcons (mkid _ "_Number" _ _) []) = true
||  isNumber _  = false

and isIdent (mktcons (mkid _ "_Ident" _ _) []) = true
||  isIdent _ = false

/* ========= Extract reserved words from a grammar =====================================*/
and wordsfromCgs (mkct ch)          = [[ch]]                                         --==
||  wordsfromCgs (mkctid s)         = [s]                                            --==
||  wordsfromCgs (mklist1 _ cgss _) = concmap wordsfromCgs cgss                      --==
||  wordsfromCgs (mklist0 _ cgss)   = concmap wordsfromCgs cgss                      --==
||  wordsfromCgs _                  = []                                             --==

and wordsfromProd (mknormal cgss _) = concmap wordsfromCgs cgss                      --==
||  wordsfromProd (mkforget cgss _) = concmap wordsfromCgs cgss                      --==

and reserved_words [] = []                                                           --==
||  reserved_words ((_,alts).gram) = concmap (\(_,prod).wordsfromProd prod) alts     --==
                                     @ reserved_words gram                           --==
/*======================================================================================*/

/* ========= Extract symbolseqs with length > 1 from a grammar =========================*/
and symsfromCgs (mkctsym s) & (length s > 1) = [s]
||  symsfromCgs (mklist1 _ cgss _)           = concmap symsfromCgs cgss
||  symsfromCgs (mklist0 _ cgss)             = concmap symsfromCgs cgss
||  symsfromCgs _                            = []

and symsfromProd (mknormal cgss _) = concmap symsfromCgs cgss
||  symsfromProd (mkforget cgss _) = concmap symsfromCgs cgss

and longsymbols [] = []
||  longsymbols ((_,alts).gram) = concmap (\(_,prod).symsfromProd prod) alts
                                @ longsymbols gram
/*======================================================================================*/

/*========== Transform lexicals =======================================================*/
and findtoken found symseq [] = found
||  findtoken found (sym.symseq) (gramch.gramrest) & (sym=gramch) 
                              = findtoken (sym.found) symseq gramrest 
||  findtoken _  _ _          = []

and longest flong (tok.rest) = if length flong > length tok 
                               then longest flong rest 
                               else longest tok rest
||  longest flong []         = flong 

and taketoken symseq gramtokens = 
      let ftokens = map (findtoken [] symseq) gramtokens in
      let tok = longest [] ftokens in
      let restsymseq = tail (length tok) symseq in 
      (rev tok,restsymseq)

and translex pflag p u gram lexl =
   let reserved = reserved_words gram in 
   let longsyms = longsymbols gram in 
   let rec trans u [] = (u,[])
       ||  trans u (c.cs) = 
          let (newc,u1,ncs)  
                = case c         
                  in mklt c'   : (mkterm c',u,cs)
                  || mkltint n : (mktermint n,u,cs)
                  || mkltid id : if mem id reserved 
                                 then (mktermreserved id,u,cs) 
                                 else (mktermid id,u,cs)
                                 -- split symbol tokens according to grammar tokens
                  || mkltsym s : case taketoken s longsyms 
                                 in ([],[ch]): (mktermsym [ch],u,cs)
                                 || ([],chs) : (mktermsym [(hd chs)],u,mkltsym (tl chs).cs)
                                 || (tok,[]) : (mktermsym tok,u,cs)
                                 || (tok,chs): (mktermsym tok,u,mkltsym chs.cs)
                                 end
                               -- typecheck if token is an unquotation
                  || mkunq e   : let (esvbSts,u1) = eW pflag p e u in 
                                                (mktyped e esvbSts , u1,cs)
                  end 
          in let (u2,newcs) = trans u1 ncs
             in (u2,newc.newcs)

    in 
        trans u lexl


/*======================================================================================*/


and endmark = '\t'        -- endmark in Earley's algorithm

-- Special eqtype used only in earley, regard different typevars as equal 
and eeqtype (mktvar n) (mktvar m) = true
||  eeqtype (mktcons i1 ts1) (mktcons i2 ts2)
                                  = eqid i1 i2 & And (map2 eeqtype ts1 ts2)
||  eeqtype _ _ = false			-- !! There are more cases!

-- true if the substitution is okey
and is_ok_sub (bad _) = false
||  is_ok_sub (ok _ _ _) = true

-- to consider polymorphic types as monomorphic -- used for precedences 
and mkmono (mktcons i _) = mktcons i []
||  mkmono other         = other 

/*
-- equality test on precedences
and preceqtrace ps ps2 = 
                    let result = preceq' ps ps2 
                    in 
                       trace (if result
                               then "precEQ"
                               else "-|EQ: "@prps ps @ " "@prps ps2
                              ) 
                              result
*/

and preceq [] [] = true
||  preceq ((t1,pass1).rest1) ((t2,pass2).rest2) =
          eeqtype t1 t2 & pass1=pass2 & preceq rest1 rest2
||  preceq _ _ = false
 
and type Try *A = Succeed *A + Fail

and mkws t p = [(mkmono t,p)]

and find A [] = Fail
||  find A ((B,x).rest) = let mA = mkmono A 
 		          in 
                            if eeqtype mA B
			    then Succeed x
     		            else find mA rest

and away B [] = []
||  away B ((A,x).rest) = let mB = mkmono B 
                          in 
                            if eeqtype mB A 
                            then away mB rest 
                            else (A,x).away mB rest

-- updates precedences,ps, with a new type,A, and prec, p. 
-- Old information about A is taken away.
and upd ps A p = let mA = mkmono A in (mA,p).away mA ps 

-- returns the list of numbers of predicted items for (A,lps,rps) if
-- any in the used-list and otherwise []
and getpredicted (A,lps,rps) [] = []
||  getpredicted (A,lps,rps) ((B,lps2,rps2,predicted).used) = 
                   if eeqtype A B & preceq lps lps2 & preceq rps rps2 
                   then predicted 
                   else getpredicted (A,lps,rps) used

------------------------------------------------------------------------------
-- isleftmost : Item -> Int -> Bool
-- true if the symbol to the right of the dot is the leftmost one
-- j is the number of the state set
and isleftmost (ITEM _ _ _ _ _ _ _ _ es _ _ _ _ _ i) j = null es & i=j

-- isrightmost : Item -> Bool
-- true if the symbol to the right of the dot is the rightmost one
and isrightmost (ITEM _ _ _ _ _ _ _ _ _ (sym.lex) _ _ _ _ _) =
      let middlesym (mklistend _ _) = false
       || middlesym (mkct c)      = c ~= endmark 
       || middlesym   _           = true
      in 
        null (filter middlesym lex)
|| isrightmost S = fail ("Cannot happen:Item "@prItem S@" in isrightmost.")  

------------------------------------------------------------------------------
-- addlistend : Int -> Item -> Item 
-- updates the states with the symbol listend after the dot
-- used to mark listend at unquotescan
and addlistend n lb (ITEM q qL A pass c b lps rps es lex vb s lws rws i)
  =  (ITEM q qL A pass c b lps rps es (mklistend n lb.lex) vb s lws rws i)

------------------------------------------------------------------------------ 
-- addpredicted : Item -> "predictlist" -> Item
-- updates a state with a list of the number of the predicted items
-- for the type after the dot.
and addpredicted (ITEM q _ A pass c b lps rps es (mkcnt B .lex) vb s lws rws i) qL
            = (ITEM q qL A pass c b lps rps es (mkcnt B .lex) vb s lws rws i) 
||  addpredicted S qL = fail ("Cannot happen: Item "@prItem S@" in addpredicted.") 
------------------------------------------------------------------------------

-- movedot_term : Item -> Item 
-- moves the dot over a terminal in a state
-- and calculate precedence weights if the terminal is the leftmost or
-- the rightmost symbol
and movedot_term (S as ITEM q qL A pass con b lps rps es (_.lex) vb s lws' _ j) =
        let lws = if isleftmost S j 
                  then [(mkmono A,0)]
                  else lws'            -- is already calculated
        in
        let rws = if isrightmost S 
                  then [(mkmono A,0)]
                  else []              -- must be something 
        in 
            (ITEM q qL A pass con b lps rps es lex vb s lws rws j)  

|| movedot_term S = fail ("Cannot happen: Item "@prItem S@" in movedot_term.")
------------------------------------------------------------------------------


-- movedot_nonterm : Item -> Int -> Texpr -> "left weights" 
--                      -> "right weights" -> "Vbind" -> Subst -> Item
--
-- moves the dot over the nonterminal to the left of the dot
-- updates the left and rights weights if the nonterminal is the
-- leftmost or the rightmost symbol.
-- k is the number of the state set (needed for leftmost symbol test)
-- e is the produced "parse tree" for the nonterminal A or list of A:s
-- Alws, Arws is weights for the produced parse tree
-- vb is a variable binding that should be added to the state
-- s is the new substitution
-- applies the substitution on the item
and movedot_nonterm (S as ITEM q qL D (p,ass) con b lps rps es (A.lex) vb' _ lws' _ i)
                    k e Alws Arws vb s
         -- calculate weights if leftmost or rightmost symbol
       = let lws = if isleftmost S k 
                   then 
                        case find D Alws in
                            Succeed lw : upd Alws D (max lw p)
                        ||   _         : upd Alws D 0 
                        end
                   else 
                        lws'     -- already calculated 
         in 
         let rws = if isrightmost S 
                   then 
                        case find D Arws in
                           Succeed rw : upd Arws D (max rw p)
                        ||   _        : upd Arws D 0 
                        end
                   else 
                        []      -- must be something
         in 
  --   trace ("Item in movedot_nonterm " @ prItem S @ "\n")
           (ITEM q qL D (p,ass) con b lps rps (es@[e])
                 (map (TRCgs s) lex) (vb@vb') s lws rws i)

|| movedot_nonterm S k e Alws Arws vb s
             = fail ("Cannot happen: Item "@prItem S@" in movedot_nonterm.")  
-----------------------------------------------------------------------------
-- precedencecheck : Item -> Int -> "left weights" -> "right weights" -> Bool
--
-- true if a parse tree  B  with weights lws and rws is accepted at the 
--                      /|\
-- place of the nonterminal B in the item D -> es.B lex , i   
--
-- k is the number of the state set (needed for leftmost symbol test)
--
and precedencecheck (S as ITEM _ _ D pass _ _ lps rps es (mkcnt B .lex) _ _ _ _ i)
                    k lws rws =
 let result = 
      let lw = case find B lws 
               in (Succeed lw) : lw
               ||   _       : fail ("Cannot happen: lw missing for "@prttype B)
               end in
      let rw = case find B rws 
               in (Succeed rw) : rw
               ||   _       : fail ("Cannot happen: rw missing for "@prttype B)
               end 
      in
        (if isrightmost S               --  D -> w.B
         then case find B (upd rps D pass) in
                Succeed(rp,ass) :  rp > lw |
                                  (rp = lw & (ass=arightassoc | ass=abothassoc))
              || Fail           : true
              end
         else true)
         &
        (if isleftmost S k              --  D -> .Bw
         then case find B (upd lps D pass) in         
                 Succeed(lp,ass) :  lp > rw | 
                                   (lp = rw & (ass=aleftassoc | ass=abothassoc))
              || Fail            : true
              end
         else true)         
  in 
    -- trace (if result then "T " else "F ")
     result

|| precedencecheck S k lws rws
        = fail ("Cannot happen: Item "@prItem S@" in precedencecheck.")  
------------------------------------------------------------------------------

-- complete : Int -> Ttype -> Texpr -> "Vbind"
--            -> Subst -> "left weights" -> "right weights"
--            -> Int -> Int -> List Item -> (Int # List Item)
-- Arguments:
-- the number on the completion state
-- the type on the left hand side in the completionstate,
-- the expression on the right hand side in the completionstate
-- the variable binding in the completion state
-- the substitution in the completion state
-- left weights of the produced parse tree
-- right weights of the produced parse tree
-- a uniq number (for new typevariables)
-- the number of the state set pointed to (needed for leftmost symbol test)
-- all states in the state set pointed to
--
-- Result:
-- a new uniq number (typevariables)     # 
-- updated states in the state set pointed to that had a dot to the left of
--   a type such that a unification with the type that has been completed
--   succeeded, and such that the precedence check was okey.
--   
--   Before completion:
--              k                                    j 
--        D -> es.A'lex ,s',i     . . . .        A -> e . ,s ,k
--
--   After completion:
--              k                                    j 
--        D -> es.A'lex ,s',i     . . . .        A -> e . ,s ,k
--                                               D -> es e.lex , s'' ,i
--                                               
--                                               s'' = (Unify A A') s s'
--
and complete1 q A e vb s lws rws u k   
              (S as (ITEM _ qL D  _ con _ _ _ es ((mkcnt A') .lex) _ s' _ _ i))
      = if mem q qL
        then 
           let (T,u1) = instTR (getTvars A) u in
           let newsubst = combTRs [Unify (T A) (TRtype s' A');
                                   instsubstTR T s ; s']
           in
            if is_ok_sub newsubst & precedencecheck S k lws rws
            then
               (u1, [(movedot_nonterm S k e lws rws vb newsubst)])
            else
               (u, [])
        else
          (u, [])
 
|| complete1 q A e vb s lws rws u k S
              = fail ("Cannot happen: Item "@prItem S@" in complete1.")
  
and complete q A e vb s lws rws u k SS = 
      let (u1,compstates) =
        mapstate (\u.\S. complete1 q A e vb s lws rws u k S) u SS
      in (u1, conc compstates)

------------------------------------------------------------------------------

and epsiloncomplete S i u epsilist = 
-- trace ("epsiloncompleted Item: " @ prItem S @ "\nEpsi :" @ prEpsi epsilist)
      let (u1,compstates) =
         mapstate (\u2.\(q,A,e,s,rws,lws).
                      complete1 q A e [] s lws rws u2 i S
                  )
                  u
                  epsilist
      in
         (u1,conc compstates)

------------------------------------------------------------------------------
		 
-- predict : "grammar" -> Ttype -> "left precs" -> "right precs"
--            -> Int -> "Used" -> "predictlabel" -> "uniq typevar" ->
--      ("uniq typevar" # "predictlabel" # List Item # "predicted" # "newused")
--          
-- fetch the productions from the grammar that have not been predicted
-- in this item set i.e. is not in "Used" and that matches the type.
-- If B is a typevariable fetch all types from the grammar that are not
-- in used with the same inherited precedences.
-- 
-- Makes items of them with left and right precedences, the dot first,
-- an empty variablebinding,
-- an empty substitution, 
-- empty (so far) left and right weights,
-- and the pointer Int.
-- Also returns a uniq typevar, a uniq predictlabel, predictlist, usedlist 
-- If B is a typevariable ......
and predict gram B lps rps i used qs u = 
     let
        mkitem A V (ccon con, mknormal cgsl pass) q u =
         let VA = TRtype V A in
         let Vcgsl = map (TRCgs V) cgsl in
         let (T,u1) = instTR (getTvars VA) u in 
           (ITEM q [] (T VA) pass con false lps rps [] 
                 (map (appCgsT T) Vcgsl) [] emptyTR [] [] i
            , u1
           )
     || mkitem A V (ccon con, mkforget cgsl pass) q u =
         let VA = TRtype V A in
         let Vcgsl = map (TRCgs V) cgsl in
         let (T,u1) = instTR (getTvars VA) u in 
           (ITEM q [] (T VA) pass con true lps rps [] 
                 (map (appCgsT T) Vcgsl) [] emptyTR [] [] i
            , u1
           )
     in
       let ((u1,q1),items_predicted_used) =
          mapstate (\(u,q).\(A,alternatives). 
                      case case B in 
                             mktvar _ : emptyTR 
      		           || B       : Unify A B 
             	           end
                      in 
                         bad _ : ((u,q),([],[],[]))
                      || s     :
                           case getpredicted (TRtype s A,lps,rps) used in 
                             [] :   -- has not been predicted before 
			          let q' = length alternatives+q-1 in
     			          let predictnums = count q q' in
                                  let (items,u2) = Umap2 (mkitem A s)
							 alternatives
                                                         predictnums
							 u
				  in 
                                     ((u2,q'+1), 
                                      (items, predictnums, 
                                       [(TRtype s A,lps,rps,predictnums)])
                                     )

                           || pnums : -- has been predicted before 
				         ((u,q),([],pnums,[]))
                           end
                      end
                   )
                   (u,qs)
                   gram
        in 
         let (items,predicted,newused) = split3 items_predicted_used
         in 
           (u1, q1, conc items , conc predicted, (used @ conc newused))
	     
------------------------------------------------------------------------------
-- eW : Bool -> Prefix -> Texpr -> Int 
--        -> (List (List Texpr # "Vbind" # Subst # Ttype) # Int)

and eW pflag p e u = 
       let et = mktvar u in 
         if pflag 
         then let (esvbVts,u1) = Wp p e et (u+1)
              in
                (map (\(es,vb,V,t). (es,TRvbind V vb,pruneTR u V,TRtype V et)) esvbVts,u1)
         else let (esVts,u1) = W p e et (u+1)
              in 
--             trace (prposs esVts)
                (map (\(es,V,t).(es,[],pruneTR u V,TRtype V et)) esVts, u1)
                

------------------------------------------------------------------------------
--	  
-- processS : Int -> Int -> List (List Item) -> "grammar" -> Int
--            -> Lex
--            -> "Epsilon-complete-list" 
--            -> "Used-list"
--            -> List Item 
--            -> (Int # List Item # List Item)
-- Arguments:
----- a uniq number for new typevars 
----- a uniq number for predictlabels 
----- the produced statesets so far
----- the grammar
----- the number on the state set that should be processed, i
----- the lexical token number i+1 
----- the states in state set i, with the dot to the left of a type,
-----    that already have been processed.
-----    (they are needed for epsilon completions)
----- the list of types with their associated "things" that have been
-----    epsilon completed in this state set
----- the list of types with inherited precedences that have been
-----    predicted in this state set
----- the states in state set i that should be processed 
-----
-- operates on item set number i until it is empty
-- returns a uniq number, the items in item set number i and number (i+1)
-- produced so far.


and processS u qs Estate gram i c Statei epsi used [] = (u,Statei,[])
||  processS u qs Estate gram i c Statei epsi used (S.States) =


--    trace ("Item in item set " @ show_int i @": "@ prItem S @ "\n" 
--        @  "Used in item set " @ show_int i @": "@ prUsed used @"\n" 
--        @  "Epsi in item set " @ show_int i @": "@ prEpsi epsi @"\n" 
--          )

     (
      case S in 

                     -- ******** SCAN ***terminal char***************             

       ITEM _ _ A _ con b lps rps es (mkct a .lex) vb s lws rws j : 
  --   trace ("Gramsym: " @ [a] @ "\nLexsym:  " @ prToken c)
           (
		let (u1,si,sii) = processS u qs Estate gram i c Statei
                                           epsi used States
		in
		    case c in
		       mkterm c'   : (u1,si,if a=c'
                                            then (movedot_term S).sii
                                            else sii
                                     )
                    || mktermint _ : (u1,si,sii)
                    || mktermid _  : (u1,si,sii)
                    || mktermreserved _  : (u1,si,sii)
                    || mktermsym _ : (u1,si,sii)
		    || mktyped _ _ : (u1,si,sii)
		    end
           )
                    -- ******** SCAN ***terminal int***************      

    || ITEM _ _ A _ con b lps rps es (mkctint a .lex) vb s lws rws j : 
   --     trace ("Gramsym: " @ show_int a @ "\nLexsym:  " @ prToken c)
              (
		let (u1,si,sii) = processS u qs Estate gram i c Statei
                                           epsi used States
		in
		    case c in
		       mkterm _   : (u1,si,sii)
                    || mktermint c' : (u1,si,if a=c' 
                                             then (movedot_term S).sii 
                                             else sii)
                    || mktermid _  : (u1,si,sii)
                    || mktermreserved _  : (u1,si,sii)
                    || mktermsym _ : (u1,si,sii)
		    || mktyped _ _ : (u1,si,sii)
		    end
              )
                    -- ******** SCAN ***terminal id***************      

    || ITEM _ _ A _ con b lps rps es (mkctid a .lex) vb s lws rws j : 
   --     trace ("Gramsym: " @ a @ "\nLexsym:  " @ prToken c)
              (

		let (u1,si,sii) = processS u qs Estate gram i c Statei
                                           epsi used States
		in
		    case c in
		       mkterm _   : (u1,si,sii)
                    || mktermint _ :  (u1,si,sii)
                    || mktermid c' : (u1,si,if a=c' 
                                             then (movedot_term S).sii 
                                             else sii)
                    || mktermreserved c' : (u1,si,if a=c' 
                                                  then (movedot_term S).sii 
                                                  else sii)
                    || mktermsym _ : (u1,si,sii)
		    || mktyped _ _ : (u1,si,sii)
		    end
              )


                    -- ******** SCAN ***terminal sym***************      

    || ITEM _ _ A _ con b lps rps es (mkctsym a .lex) vb s lws rws j : 
      --  trace ("Gramsym: " @ a @ "\nLexsym:  " @ prToken c)
              (

		let (u1,si,sii) = processS u qs Estate gram i c Statei
                                           epsi used States
		in
		    case c in
		       mkterm _   : (u1,si,sii)
                    || mktermint _ :  (u1,si,sii)
                    || mktermid _ : (u1,si,sii)
                    || mktermreserved _ : (u1,si,sii)
                    || mktermsym c' : (u1,si,if a=c' 
                                             then (movedot_term S).sii 
                                             else sii)
		    || mktyped _ _ : (u1,si,sii)
		    end
              )


                     -- ******** PREDICT, ANTIQUOTESCAN, EPSILONCOMPLETE *******

    || ITEM _ _ A pass con b lps rps es (mkcnt B.lex) vb s lws rws j : 
         let lps2 = if isleftmost S i 
                    then away B (upd lps A pass)
                    else [] in

         let rps2 = if isrightmost S 
                    then away B (upd rps A pass) 
                    else [] in
              
         let (u1,qn,predictS,predicted,newused) = predict gram B lps2 rps2 i used qs u in
         let pS = addpredicted S predicted in
         let (u2,epsiloncompleteS) = epsiloncomplete pS i u1 epsi in 
	 
         let (u3,si,sii) = processS u2 qn Estate gram i c
                                    (pS.Statei)
                                    epsi
		    	            newused
                                    (States @ predictS @ epsiloncompleteS)
         in 
	   case c in 
               mktermint n & (isNumber B) : (u3,si,(movedot_nonterm S i 
                                                               (const (itos n) Tint ITint n) 
                                                               [(B,0)] [(B,0)] [] s).sii)

           ||  mktermid id & (isIdent B) :
                                   (u3,si,(movedot_nonterm S i
                                                           (convstr id)
	                                                   [(B,0)] [(B,0)] [] s).sii)
           ||  x & (isterminal x)  : (u3,si,sii)  
 
	   || mktyped e esvbSts :                -- ANTIQUOTESCAN
                        let B' = if isNumber B 
                                 then Tint 
                                 else if isIdent B 
                                      then Tstring  
                                      else B in 
                        let newitems =
                             concmap (\(eseq,vb1,s1,t).
                                         case combTRs [s1 ; Unify t B' ; s]
                                         in bad _ : []
                                         || s2    :
                                             [movedot_nonterm S i 
                                                              (substear eseq e)
                                                              [(B,0)] [(B,0)]
                                                              vb1
                                                              s2
                                             ]
                                         end
                                     )
                                      esvbSts
			in 
   		            (u3,si,newitems @ sii)

           end

                           -- ******************* COMPLETE *******

    || ITEM q _ A pass con b lps rps es [] vb s lws rws j : 
        let (epsi2,(u1,compstates)) =
           let e = if b then hd es else mkconstr con es
	   in 
	      if i=j                              -- epsiloncomplete 
	      then 
                 let nlws = upd lws A 0 in
                 let nrws = upd rws A 0 in 
                  ((q,A,e,s,nlws,nrws).epsi,
                    complete q A e vb s nlws nrws u j Statei
                  )
	      else
                 (epsi, complete q A e vb s lws rws u
                                     j (select (j+1) (rev Estate))
                 )			 
	in 
	  processS u1 qs Estate gram i c Statei epsi2 used (States @ compstates)


                           -- ********************** LIST1 *******

    || ITEM q qL A pass con b lps rps es (mklist1 C cgsl n .lex) vb s lws rws j :
        let (u1,si,sii) =
          processS u qs Estate gram i c Statei epsi used 
	          ((ITEM q qL A pass con b lps rps es
                         (mkcnt C.mklistend (n+1) false.lex) vb s lws rws j
                   ).
	           (ITEM q qL A pass con b lps rps es
                       (mkcnt C.(cgsl @ ((mklist1 C cgsl (n+1). lex))))
                       vb s lws rws j
                   )
	           . States
		  )
        in 
         case c 
         in x & (isterminal x) :  (u1,si,sii)
         || mktyped e esvbSts : 
              let C' = if isNumber C 
                       then Tint 
                       else if isIdent C 
                            then Tstring  
                            else C in        
              let newitems =
                 concmap (\(eseq,vb1,s1,t).
                            let s2 = combTRs [s1 ; Unify t (Tlist C'); s]
                            in case s2 
                               in bad _  : []
                               || ok _ _ _ : [addlistend (n+1) true
                                             (movedot_nonterm S i
                                                              (substear eseq e)
							      (mkws C 0)
							      (mkws C 0)
                                                              vb1
                                                              s2
                                             )
                                           ]
                                 end
                           )
                           esvbSts
              in 
  	         (u1,si,newitems @ sii)
         end

                           -- ********************** LIST0 *******
    || ITEM q qL A pass con b lps rps es (mklist0 C cgsl .lex) vb s lws rws j :
         processS u qs Estate gram i c Statei epsi used 
                 ((movedot_nonterm S i (mkconstr hcnil []) (mkws C 0) (mkws C 0) [] s
                  ).
                  (ITEM q qL A pass con b lps rps es 
                        (mklist1 C cgsl 0.lex) vb s lws rws j
                  ). States
                 )

                             -- *********************** LISTend *******
     -- all elements in es is element in the list 
  || ITEM q qL A pass con b lps rps es (mklistend n false.lex) vb s lws rws j :
        let newes = 
	 let i = length es - n
	 in  
	    head i es @ [reduce (\e.\l.mkconstr hccons [e;l])
                                (mkconstr hcnil [])
                                (tail i es)
                        ]
        in 
	  processS u qs Estate gram i c Statei epsi used 
            ((ITEM q qL A pass con b lps rps newes lex vb s lws rws j).States)

     -- the last element in es is a list 
  || ITEM q qL A pass con b lps rps es (mklistend n true.lex) vb s lws rws j :
       let newes = 
	 let i = length es - n
	 in  
	    head i es @ [reduce1 (\e.\l.mkconstr hccons [e;l])
                                 (tail i es)
                        ]
        in 
	  processS u qs Estate gram i c Statei epsi used 
            ((ITEM q qL A pass con b lps rps newes lex vb s lws rws j) . States)

    end -- case 
) -- traceparentes
------------------------------------------------------------------------------

-- Produce: Bool -> Prefix -> Int -> "grammar" -> List (List Item)
--           -> Int -> List Lex
--            -> (Int # List Item)
--Arguments:
---- true if it is a pattern that is parsed
---- prefix (typeenvironment)
---- a uniq number 
---- the grammar
---- the produced statesets so far
---- the number on the stateset that should be processed
---- lexical symbols
-- Returns:
---- a uniq number # the states in the last produced stateset
--    
and Produce u gram (S.SS) _ [] =  (u,S)             -- no more input
||  Produce u gram ([]._) _ _ = (u,[])              -- syntax error
||  Produce u gram (S.SS) i (c.cs) = 

	let (u2,Si,Sii) = processS u 0 SS gram i c [] [] [] S 
	in 

/*           trace ("Item set "@show_int i@" after processS\n" @
                   concmap prItem Si @ "\n" @
                  "Item set "@show_int (i+1) @" before processS \n" @
                   concmap prItem Sii @ "\n") 
*/
                 ( 
    	           Produce u2 gram (Sii.Si.SS) (i+1) cs
                 )
------------------------------------------------------------------------------
-- earley : Bool -> Prefix -> "grammar" -> Ttype -> List Lex -> Int -> 
--           -> (List (Texpr # Subst # Ttype) # Int)
and earley pflag p gram et lexl u = 
     let (u2,startitems) =        
        let
            mkitem A V u (ccon con, mknormal cgsl pass) =
             let (T,u1) = instTR (getTvars A) u in
             let V1 = instsubstTR T V in 
   	       (u1, ITEM 0 [] (TRtype V1 (T A)) pass con false [] [] []
                         ((map (instCgsTR T V1) cgsl)@[mkct endmark]) [] V1 [] [] 0
               )
        ||  mkitem A V u (ccon con, mkforget cgsl pass) =
             let (T,u1) = instTR (getTvars A) u in
             let V1 = instsubstTR T V in 
   	       (u1, ITEM 0 [] (TRtype V1 (T A)) pass con true [] [] []
                         ((map (instCgsTR T V1) cgsl)@[mkct endmark]) [] V1 [] [] 0
               )             

        in
          let (u2,items) = 
             mapstate (\u1.\(A,alternatives).
                          case Unify A et
                          in bad _ : (u1,[])
                          || V     : mapstate (mkitem A V) u1 alternatives
  		          end
                      )
                      u
                      gram
          in 
              (u2, conc items)
     in
--      trace ("\nStart items:\n" @ concmap prItem startitems)
      (let (u3,newlexl) = translex pflag p u2 gram lexl in
       let (u1,lastitemset) = Produce u3 gram [startitems] 0 (newlexl@[mkterm endmark])
       in 
         let rec f (ITEM _ _ t _ con b _ _ es [] vb s _ _ _) = 
                  ( (if b then hd es else mkconstr con es),
                    (TRvbind s vb),
                     pruneTR u s,
                     TRtype s t
                  )
          || f item = fail ("Wrong item in last item set :" @ prItem item )

         and remdubl [] = []
          || remdubl ((p as (e,vb,S,t)).rest) = 
                      if member eqTexpr e (map fstof4 rest) 
                      then remdubl rest
                      else p.(remdubl rest)

         in 
--           trace ("\nLast item set:\n" @ concmap prItem lastitemset)
                 (remdubl (map f lastitemset) , u1) 
      )
end -- module 
