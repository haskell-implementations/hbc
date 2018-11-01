module
#include "../misc/flags.t"
#include "../flic/flic.t"
#include "../expr/read.t"
#include "../zf/lmlzf.t"
#include "../curry/curry0.t"
#include "../rename/rename.t"
#include "../rename/renenv.t"
#include "../ilml/icomp.t"
#include "files.t"
export errmap, expr0, expr1, expr2, unum1, allinsts, startu, preenv, pragmas, curmod;
rec (expr0, errmap) = if InFlic then (flic finput, []) else Read finput
and  expr1 = if Curry then curry0 expr0 else lmlzf expr0
and (u1, allinsts, pe, pragmas, curmod, expr2) = rename startu expr1
and unum1 = if Interactive then i_unum1 else u1
and preenv = if Interactive then i_preenv else rperm pe
and startu = 1000
end
