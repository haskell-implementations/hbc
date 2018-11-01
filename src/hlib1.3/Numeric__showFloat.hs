module Numeric__showFloat where

import Numeric__formatRealFloat
showFloat x = showString (formatRealFloat FFGeneric Nothing x)

