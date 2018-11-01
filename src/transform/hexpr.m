module -- hexpr
-- Useful expressions
#include "../misc/flags.t"
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../expr/constr_t.t"
#include "../rename/renenv.t"
#include "../rename/import.t"
#include "../main/topexprs.t"
#include "cutil.t"

-- There is a serious problem with these identifers: they have not
-- been through the final renaming phase that adds the class and instance
-- info.  BEWARE!!

export hiand, hiadd, hisub, hige, hieq, hctrue, hcfalse,
       hilt, hile, hior, hileint, 
       hiEq, hiOrd, hiIx, hiEnum, hiText, hiBinary, hiShow, hiRead,
       hiBounded, hiEval, hiFunctor,
       himinBound, himaxBound,
       hiARROW, hicno, hifail, hieqint, hiord, hiltint, hiCPointer,
       hirange, hiindex, hiinRange, hishowsPrec, hireadsPrec, hishowsType,
       hishowString, hishowParen, hishowChar, hicomp, hidol, hiflip,
       hiPrange, hiPindex, hiPinRange, hinever, hishowt,
       hienumFrom, hienumFromThen, hienumFromTo, hienumFromThenTo,
       hienumFT, hienumFTT, hienumFTTU, --hienumTF, hienumTFT,
       hireadBin, hishowBin, hiPreadBin, hiPshowBin,
       hitrue, hifalse, hicons, hinil, hiDialogue, hiRequest, hiResponse, hiIO, hiUnit,
       hireadRec, himapFst, hireads,
       hifromInteger, hifromRational, hirangeSize,
       hipair, hcpair, himul, hiList, hiInt, hiChar, hiBool, hiSFloat, hiInteger, hiComplex,
       hcnil, hccons, hcunit, higt, hine, hinegate, hi_ord, hi_chr, hiseq, hidiv, himod,
       hiDFloat, stdClass, numClass, hiNum, hiRational,
       hinot, hiotherwise, hiString, hiIntegral,
       hitoEnum, hifromEnum, hichr, hicompare, hiEQ, hiLT, hiGT, hcEQ, hcGT, hcLT,
       hiconc, hireadParen, hilex, hiundefined, hi_undefined,
       hituple, isidfail, hidocmp,
       hiband, hibor, hibxor, hibcompl, hiblsh, hibrsh, hibrsha,
       hidfadd, hidfsub, hidfmul, hidfdiv, hidfnegate, hidftoi, hiitodf,
       higeneq, higenne, higenlt, higenle, higenge, higengt,
       hizip, hizip3, hizip4, hinexp, hit3, hit4;

rec vf s = xxxxx (rfind Kvalue (mkps s) preenv) s
and vt s = xxxxx (rfind Ktype (mkps s) preenv) s
and mkps s = case mkpids s in mkids s : s end
and xxxxx i s = if id_no i = 0 & Curry then fail ("predef-hexpr "@s@"="@mkps s@" undef\n"@show_Renv preenv) else i --!!!
and vfc s1 s2 = if Curry then vf s1 else vf s2
and vtc s1 s2 = if Curry then vt s1 else vt s2
and hiand      = vf "_&&"
and hior       = vf "_||"
and hinot      = vf "_not"
and hiotherwise= vf "_otherwise"
and hiadd      = vf "_+"
and himul      = vf "_*"
and hisub      = vf "_-"
and hidiv      = vfc "_div" "_/"
and himod      = vfc "_mod" "_%"
and hinegate   = vf "_negate"
and hi_ord     = if H1_3 then vf "P_ord" else vf "_ord"
and hi_chr     = if H1_3 then vf "P_chr" else vf "_chr"
and higt       = vf "_>"
and hige       = vf "_>="
and hilt       = vf "_<"
and hile       = vf "_<="
and hicompare  = vf "_compare"
and hiEQ       = vf "_EQ"
and hiLT       = vf "_LT"
and hiGT       = vf "_GT"
and hieq       = vfc "_==" "_="
and hine       = vfc "_/=" "_~="
and hicomp     = vfc "_." "_o"
and hidol      = vf "_$"
and hiflip     = vf "_flip"
and hirange    = vf "_range"
and hiindex    = vf "_index"
and hiinRange  = vf "_inRange"
and hienumFrom = vf "_enumFrom"
and hienumFromThen = vf "_enumFromThen"
and hienumFromTo   = vf "_enumFromTo"
and hienumFromThenTo = vf "_enumFromThenTo"
and hitoEnum   = vf "_toEnum"
and hifromEnum = vf "_fromEnum"
and himinBound = vf "_minBound"
and himaxBound = vf "_maxBound"
and hireadBin  = vf "_readBin"
and hishowBin  = vf "_showBin"
and hishowsPrec= vf "_showsPrec"
and hireadsPrec= vf "_readsPrec"
and hishowsType= vf "_showsType"
and hishowString = vf "_showString"
and hishowParen = vf "_showParen"
and hishowChar = vf "_showChar"
and hifromInteger = vf "_fromInteger"
and hifromRational = vf "_fromRational"
and hirangeSize = vf "_rangeSize"
and hierror    = vf "_error"
and hiundefined= vf "_undefined"
and hi_undefined= vf "__undefined"
and hicno      = vf "Pcno"
and hifail     = vf "Pfail"
and hieqint    = vf "Peqi"
and hiltint    = vf "Plti"
and hileint    = vf "Plei"
--and hieqe      = vf "Peqe"
--and hilte      = vf "Plte"
--and hilee      = vf "Plee"
and hiord      = vf "Pord"
and hichr      = vf "Ptag"
and hiPrange   = vf "Prange"
and hiPindex   = vf "Pindex"
and hiPinRange = vf "PinRange"
--and hienumTF   = vf "PenumTF"
--and hienumTFT  = vf "PenumTFT"
and hienumFT   = vf "PenumFT"
and hienumFTT  = vf "PenumFTT"
and hienumFTTU = vf "PenumFTTU"
and hiPreadBin = vf "PreadBin"
and hiPshowBin = vf "PshowBin"
and hinever    = vf "Pnever"
and hishowt    = vf "Pshowt"
and hiseq      = vf "Pseq"
and hitrue     = vfc "_True" "_true"
and hifalse    = vfc "_False" "_false"
and hicons     = vfc "_:" "_."
and hinil      = vf "_[]"
and hiunit     = vf "_()"
and hipair     = vf "P#2"
and hit3       = vf "P#3"
and hit4       = vf "P#4"
and hiconc     = vfc "_++" "_@"
and hireadParen= vf "_readParen"
and hilex      = vf "_lex"
and hireadRec  = vf "__readRec"
and himapFst   = vf "__mapFst"
and hireads    = vf "_reads"

and hizip      = vf "_zip"
and hizip3     = vf "_zip3"
and hizip4     = vf "_zip4"

and hinexp     = vf "_^"

and hidocmp    = vf "Pdocmp"
and hiband     = vf "_bitand"
and hibor      = vf "_bitor"
and hibxor     = vf "_bitxor"
and hibcompl   = vf "_bitcompl"
and hiblsh     = vf "_bitlsh"
and hibrsh     = vf "_bitrsh"
and hibrsha    = vf "_bitrsha"

and higeneq    = vf "Pgeneq"
and higenne    = vf "Pgenne"
and higenlt    = vf "Pgenlt"
and higenle    = vf "Pgenle"
and higenge    = vf "Pgenge"
and higengt    = vf "Pgengt"

and hidfadd      = vf "_+."
and hidfmul      = vf "_*."
and hidfsub      = vf "_-."
and hidfdiv      = vf "_/."
and hidfnegate   = vf "PFloatNeg"
and hidftoi      = vf "Pftoi"
and hiitodf      = vf "Pitof"

and hcEQ       = idtoconstr hiEQ
and hcLT       = idtoconstr hiLT
and hcGT       = idtoconstr hiGT
and hctrue     = idtoconstr hitrue
and hcfalse    = idtoconstr hifalse
and hcpair     = idtoconstr hipair
and hcnil      = idtoconstr hinil
and hccons     = idtoconstr hicons
and hcunit     = idtoconstr hiunit
and hiCPointer = vt "__CPointer"
and hiEq       = vt "_Eq"
and hiOrd      = vt "_Ord"
and hiIx       = vt "_Ix"
and hiEnum     = vt "_Enum"
and hiText     = vt "_Text"
and hiShow     = vt "_Show"
and hiRead     = vt "_Read"
and hiBinary   = vt "_Binary"
and hiBounded  = vt "_Bounded"
and hiFunctor  = vt "_Functor"
and hiEval     = vt "_Eval"
and hiARROW    = vt "P->"
and hiDialogue = vt "_Dialogue"
and hiRequest  = vt "_Request"
and hiResponse = vt "_Response"
and hiIO       = vt "_IO"
and hiUnit     = vt "_()"
and hiString   = vt "_String"
and hiList     = vtc (if H1_3 then "_[]" else "PList") "_List"
and hiInt      = vt "_Int"
and hiInteger  = vt "_Integer"
and hiRational = vt "_Rational"
and hiSFloat   = vt "_Float"
and hiChar     = vt "_Char"
and hiBool     = vt "_Bool"
and hiDFloat   = vt "_Double"
and hiComplex  = vt "_Complex"
and hiNum      = vt "_Num"
and hiReal     = vt "_Real"
and hiFractional = vt "_Fractional"
and hiIntegral = vt "_Integral"
and hiRealFrac = vt "_RealFrac"
and hiFloating = vt "_Floating"
and hiRealFloat= vt "_RealFloat"
and hituple n  = if H1_3 then vt ('P'.'#'.itos n) else vt ('_'.'#'.itos n)

and stdClass   = [hiEq; hiOrd; hiIx; hiEnum] @ (if H1_3 then [hiBounded; hiShow; hiRead; hiFunctor] else [hiBinary; hiText]) @ if EvalClass then [hiEval] else []
and numClass   = [hiNum; hiReal; hiFractional; hiIntegral; hiRealFrac; hiFloating; hiRealFloat]

and isidfail i = eqid i hifail | Curry & eqid i hierror

end
