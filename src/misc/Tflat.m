module
#include "text_t.t"
export Tflat;
rec Tflat (Ti i) c = itos i @ c
 || Tflat (Ts s) c = s @ c
 || Tflat (Tl l) c = itlist (\t.\p.Tflat t p) l c
end
