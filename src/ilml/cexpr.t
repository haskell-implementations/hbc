import show_Cexpr: (Cexpr->(List Char)) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import type Tconst = CInt Int + CChar Char + CString String + CSFloat String + CInteger String + CDFloat String;
import type Cexpr = Lam Id Cexpr + Var Id + Ap Cexpr Cexpr + Comb String Univ + Constant Tconst + Fail + Konstr Int Int (List Bool);
