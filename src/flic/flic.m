module
#include "fread.t"
#include "ftrans.t"
#include "fconv.t"
#include "fprint.t"
export flic, Etofprint;

    flic l = ftrans (fread l)
and 
    Etofprint e_module = fprint_module (Etof e_module)
end
