module I_Floating_Double where

import LML_Double

instance  Floating Double  where
    pi			=  3.141592653589793238
    exp x		=  {-:"Dexp":-} x
    log x		=  {-:"Dlog":-} x
    sqrt x		=  {-:"Dsqrt":-} x
    sin x		=  {-:"Dsin":-} x
    cos x		=  {-:"Dcos":-} x
    tan x		=  {-:"Dtan":-} x
    asin x		=  {-:"Dasin":-} x
    acos x		=  {-:"Dacos":-} x
    atan x		=  {-:"Datan":-} x
    sinh x		=  {-:"Dsinh":-} x
    cosh x		=  {-:"Dcosh":-} x
    tanh x		=  {-:"Dtanh":-} x
--    asinh x		=  {-:"Dasinh":-} x
--    acosh x		=  {-:"Dacosh":-} x
--    atanh x		=  {-:"Datanh":-} x
#define log {-:"Dlog":-}
#define sqrt {-:"Dsqrt":-}
-- BAD NUM
    asinh x = log (x + sqrt (1+x*x))
    acosh x = log (x + (x+1) * sqrt ((x-1)/(x+1)))
    atanh x = log ((x+1) / sqrt (1 - x*x))
