import type Argpos = APframe + APregister;
import type Arginfo = AInothing + AIeval + AIunboxed;
import type Finfo = finfo Int (List (List (Arginfo # Argpos))) (BT # BT) Int (Option Texpr);
import type Teinfo = strict + noeval + restr (List Int) Ttype + inline + spark (List Id) + doeval (List Id) + chkind + trestr Ttype + srestr Ttype + noarrow Ttype + notchk + forcearity Int + overload + metcall + vecreg2 + vectordef + limitok + unboxedvar + unboxedarg + unboxedexpr + unboxvars + specialcall (List (Arginfo # Argpos)) + notconst + position String Int + stmtpat + stmtfailpoint Texpr + dropctx Ttype (List Int);
