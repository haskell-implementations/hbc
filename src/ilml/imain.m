module
#include "../misc/flags.t"
#include "../misc/ioc.t"
#include "../main/version.t"
#include "toplev.t"
#include "../rename/renenv.t"
#include "icomp.t"
#include "imisc.t"

export imain;

imain =
    appendChan stdout (CUNBUFF.CINTROFF."Welcome to interactive "@ (if Curry then "Haskell B. " @ if H1_3 then "1.4" else "1.2" else "LML") @ " " @ version @ "!\n" @

    "Loading prelude... "@itos nvalues@" values"@(if ReallyLoadShareLib then", "@itos nlibs@" libraries" else "")@", "@itos (length (rids Ktype i_ipreenv))@" types found.\n"@
    "Type \"help;\" to get help.\n") exit 
    (readChan stdin exit topstart)

end
