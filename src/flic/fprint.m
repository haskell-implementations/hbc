module
#include "Flic_t.t"
#include "fesc.t"
#include "pp.t"

export fprint, fprint_module;

/************************************************************************
*									*
*		Main printing stuff					*
*									*
************************************************************************/
rec
    -- Print a top-level letrec for the definitions in defs
    fprint_module defs
    = fdisplay (pplet "&" (ppparen (mix (map (pp o name o fst) defs) " "))
			  (ppparen (mix (map (fprintE1 o snd) defs) " "))
			  "\n"	-- No "expression to evaluate"
	       )
and
    /* pretty print a Flic expr */
    fprint  =  fdisplay  o  fprintE
and
    fdisplay = ppshow 80 0  o  pp
and
    /* print a Flic expr (E in grammar) */
    fprintE (Fap a b)      =  ppparen (fprintE0 (Fap a b))
||  fprintE (Flam n a)     =  pp ("\\ " @ name n) @ " " @ fprintE a
||  fprintE (Flet is_rec ns es a)
		  =  pplet (if is_rec then "&" else "=")
			   (ppparen (mix (map (pp o name) ns) " "))
			   (ppparen (mix (map fprintE1 es) " "))
			   (fprintE a)
    /* else */
||  fprintE e              =  fprintE0 e
and
    /* print a Flic expr (application) */
    fprintE0 (Fap a b)     =  fprintE0 a @ " " @ fprintE1 b
    /* else */
||  fprintE0 e             =  fprintE1 e
and
    /* print a Flic expr (E1 in grammar) */
    fprintE1 (Fap a b)     =  ppparen (fprintE0 (Fap a b))
||  fprintE1 (Fannot a b)  =  pp (fprintA a @ fprintE1 b)
||  fprintE1 (Fname n)     =  pp (name n)
||  fprintE1 (Fnumber i)   =  pp (number i)
||  fprintE1 (Fchar c)     =  pp ("\'" @ escchar c @ "\'")
||  fprintE1 (Fstring s)   =  pp ("\"" @ concmap escstring s @ "\"")
||  fprintE1 Ffail         =  pp "FAIL"
    /* else */
||  fprintE1 e             =  ppparen (fprintE e)
    /* The cases handled by the above functions must be total
       -- otherwise, an infinite loop could result. */
and
    /* print an annotation */
    fprintA (Annot0 n)     =  ppbrack (name n)
||  fprintA (Annot1 n e)   =  ppbrack (name n @ " " @ fprintE1 e)


/************************************************************************
*									*
* 		Pretty print utilities 					*
*									*
************************************************************************/
and
    ppparen a          =  pp ("(" @ a @ ")")
and
    ppbrack a          =  pp ("[" @ a @ "]")
and
    pplet a b c d      =  pp (a @ " " @ b @ " " @ c) @ " " @ d


/************************************************************************
*									*
*		Displaying names and numbers				*
*									*
************************************************************************/

and
    /* printing characters with escapes */
    escname1   =  esc "\\=&()[] #\'\"0123456789"
and
    escname    =  esc "\\()[] #\'\""
and
    escchar    =  esc "#\'"
and
    escstring  =  esc "#\""
and
    /* names and numbers */
    name []      =  fail "empty name"
||  name (x.xs)  =  escname1 x @ concmap escname xs
and
    number i     =  if  i >= 0  then  itos i  else  itos (-i) @ "-"
end
