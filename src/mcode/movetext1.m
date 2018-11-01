module
#include "mcodedef_t.t"
export movetext1;
rec
    movetext1 ms = f ms []
and f [] [] = []
||  f (Mfunend.ms) rs = reverse rs @ Mfunend.f ms []
||  f (Mtext1.ms) rs =
	let (r,ms') = skip ms []
	in f ms' (r@rs)
||  f (m.ms) r = m.f ms r
and skip (Mtext.ms) rs = (rs, ms)
||  skip (m.ms) rs = skip ms (m.rs)
end
