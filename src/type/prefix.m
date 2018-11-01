module --prefix
#include "../expr/id.t"
#include "../expr/id_t.t"
#include "../expr/ttype_t.t"
#include "../expr/ttype.t"
#include "../misc/util.t"
#include "subst.t"

export pfind, TRprefix, startpre, prpre,
	addpre, addgs, addngs, addgens, addngens,
	addpreng, getngs, getngp, innongen, getgp;
local
    -- The prefix contains two main parts, the completely generic prefix, and the non-generic prefix.
    -- It's split for efficiency, only the non-generic parts needs substitutions in TRprefix.
    type Prefix = Pre (List(Id#(Ttype#(List Int)))) (List(Id#(Ttype#(List Int))))
in
rec
    pfind i (Pre gp ngp) =
let rrr =
        case type_of_id i in
	    Ohastype t tv _ : (t, tv)
	||  Onotype : assocdefeq eqid i ngp (assocdefeq eqid i gp (fail ("pfind "@idtostr i)))
        end
in
--trace ("pfind "@prid i@"("@itos (id_no i)@") is "@prttype (fst rrr))
rrr
and innongen i (Pre _ ngp) = member eqid i (map fst ngp)
and prpre (Pre gp ngp) = ("Gen:\n"@ f gp @ "Nongen:\n"@ f ngp
	where f = concmap (\(a,(b,c)).prid a@" :: "@prttype b@" "@show_list itos c@"\n"))
and TRprefix T (Pre gp ngp) =
	Pre gp (map (\(a,(b,c)).(a, (TRtype T b, c))) ngp)
and startpre = Pre [] []
and getgp (Pre gp _) = gp
and getngp (Pre _ ngp) = ngp
and addgens ps (Pre gp ngp) = Pre (ps@gp) ngp
and addngens ps (Pre gp ngp) = Pre gp (ps@ngp)
and addgs ps (Pre gp ngp) = Pre (map (\(a,b).(a,(b,[]))) ps @ gp) ngp
and addngs ps (Pre gp ngp) = Pre gp (map (\(a,b).(a,(b,[]))) ps @ ngp)
and getngs (Pre _ ngp) = allfree ngp []
-- this operation could be speeded up by caching this info!
and allfree ngp ng = reduce union ng (map (\(_, (t, vs)).difference (getTvars t) vs) ngp)
and addpre pl p = addpreng pl p []
and addpreng pl (Pre gp ngp) ng = (f (allfree ngp ng) gp ngp pl
	where rec f tvars gp ngp [] = Pre gp ngp
	       || f tvars gp ngp ((a, b).l) =
		    let tv = getTvars b in
	       	    let ftv = difference tv tvars in
		    if tv = ftv then f tvars ((a, (b, ftv)).gp) ngp l
		                else f tvars gp ((a, (b, ftv)).ngp) l
	)
end
end
