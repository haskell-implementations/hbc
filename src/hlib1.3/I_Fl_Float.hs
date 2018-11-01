module PreludeX where
import LML_Double
#define FTOD {-:"PFloat2Double":-}
#define DTOF {-:"PDouble2Float":-}
#define WRAP(f) (DTOF . f . FTOD)

instance  Floating Float  where
    pi			=  3.141592653589793238
    exp x		=  WRAP({-:"Dexp":-}) x
    log x		=  WRAP({-:"Dlog":-}) x
    sqrt x		=  WRAP({-:"Dsqrt":-}) x
    sin x		=  WRAP({-:"Dsin":-}) x
    cos x		=  WRAP({-:"Dcos":-}) x
    tan x		=  WRAP({-:"Dtan":-}) x
    asin x		=  WRAP({-:"Dasin":-}) x
    acos x		=  WRAP({-:"Dacos":-}) x
    atan x		=  WRAP({-:"Datan":-}) x
    sinh x		=  WRAP({-:"Dsinh":-}) x
    cosh x		=  WRAP({-:"Dcosh":-}) x
    tanh x		=  WRAP({-:"Dtanh":-}) x
--    asinh x		=  {-:"Dasinh":-} x
--    acosh x		=  {-:"Dacosh":-} x
--    atanh x		=  {-:"Datanh":-} x
#define log WRAP({-:"Dlog":-})
#define sqrt WRAP({-:"Dsqrt":-})
-- BAD NUM
    asinh x = log (x + sqrt (1+x*x))
    acosh x = log (x + (x+1) * sqrt ((x-1)/(x+1)))
    atanh x = log ((x+1) / sqrt (1 - x*x))
