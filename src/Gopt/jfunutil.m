module
#include "../expr/id.t"
#include "../Gcode/Gcodedef_t.t"
export reflist, delref, refcnt, eqGs;
rec
    labels (JMP (Label n)) = [n]
||  labels (JFALSE (Label n)) = [n]
||  labels (JTRUE (Label n)) = [n]
||  labels (CASE _ ils (Label n)) = n . map (\(_,_,Label n).n) ils
||  labels (CASE _ ils _) = map (\(_,_,Label n).n) ils
||  labels _ = []
and reflist = concmap labels
and dref n [] = fail "delref"
||  dref n (r.rs) = if n = r then rs else n.dref n rs
and delref n rs = if n <= 0 then rs else dref n rs
and refc1 n [] = 0
||  refc1 n (r.rs) = (if n = r then 1 else 0) + refc1 n rs
and refcnt n l = if n = -1 then 0 else if n = 0 then 1 else refc1 n l
-- Avoid comparing Id!
and eqG (PUSHGLOBAL i1) (PUSHGLOBAL i2) = eqid i1 i2
||  eqG (JGLOBAL n1 i1) (JGLOBAL n2 i2) = eqid i1 i2 & n1=n2
||  eqG (CALLGLOBAL n1 i1 ns1) (CALLGLOBAL n2 i2 ns2) = eqid i1 i2 & n1=n2 & ns1 = ns2
||  eqG (SCALLGLOBAL n1 i1) (SCALLGLOBAL n2 i2) = eqid i1 i2 & n1=n2
||  eqG (RECBLOCK gss1) (RECBLOCK gss2) = And (map2 eqGs gss1 gss2)
||  eqG (CONSTBLOCK gs1) (CONSTBLOCK gs2) = eqGs gs1 gs2
||  eqG (MKAPLV i1 n1) (MKAPLV i2 n2) = eqid i1 i2 & n1=n2
||  eqG g1 g2 = g1 = g2
and eqGs [] [] = true
||  eqGs (g1.gs1) (g2.gs2) = eqG g1 g2 & eqGs gs1 gs2
||  eqGs _ _ = false
end
