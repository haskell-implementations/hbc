import type Stmt = mksexp Texpr + mksexpstmt Texpr Stmt + mkslet Binding Stmt + mksbind Texpr Texpr Stmt;
import type Import = mkimport Id (List Impid) (List Fixid) (List Impid) Bool (List Expid) (List (Id # Id)) Bool (Option Id);
import type Fixid = mkfixid (List Id) Fixity;
import type Qual = mkqgen Texpr Texpr + mkqfilter Texpr + mkqlet Binding;
import type Const = cint Int + cchar Char + cstring (List Char) + cfloat (List Char) + cinteger (List Char) + crational (List Char);
import type Atype = mkcons Id (#3 Bool (List Int) (List Assert)) (List (#3 Ttype Bool (Option Id))) Bool;
import type Lex = mklt Char + mkltint Int + mkltid (List Char) + mkltsym (List Char) + mkunq Texpr;
import type Cgs = mkcnt Ttype + mkct Char + mkctint Int + mkctid (List Char) + mkctsym (List Char) + mklist1 Ttype (List Cgs) Int + mklist0 Ttype (List Cgs) + mklistend Int Bool;
import type Prod = mknormal (List Cgs) (Int # Asstype) + mkforget (List Cgs) (Int # Asstype);
import type Asstype = arightassoc + aleftassoc + anonassoc + abothassoc;
import type IdOrConstr = ccon Constr + cid Id;
import type Binding = mkbtype Ttype (List Atype) (Option (List Id)) Bool + mkbctype Ttype (List Prod) + mkbpat (List (Texpr # Texpr)) + mkbmulti Texpr Texpr + mkband Binding Binding + mkbrec Binding + mkberror (List Char) + mkblocal Binding Binding + mkbnull + mkbsyn Ttype Ttype + mkbclass CDecl Binding + mkbinstance IDecl Binding (Option Id) + mkbdefault (List Ttype) + mkbsign (List Id) Ttype + mkbpragma Pragma + mkbview Ttype Ttype (List Atype) Binding;
import type Texpr = mkap Texpr Texpr + mklam Texpr Texpr + mkcase Texpr (List (Texpr # Texpr)) + mkletv Binding Texpr + mkident Id + mkmodule Id (List Fixid) (List Impid) (Option (List Expid)) Binding + mkconst Const + mkbrack (List (Ttype # (List (IdOrConstr # Prod)))) (List Lex) + mkerror (List Char) + mkas Id Texpr + mkcondp Texpr Texpr + mklazyp Texpr + mkconstr Constr (List Texpr) + mkfailmatch Int + mkinfo Teinfo Texpr + mklistf Int (List Texpr) + mklistg Texpr (List Qual) + mkhmodule Id (Option (List Expid)) (List Import) (List Fixid) Binding + mkwhere (List (Texpr # Texpr)) Binding + mkcfunction Bool Id + mkdo Stmt + mkrecord Texpr (List (Id # Texpr)) (List Atype);
