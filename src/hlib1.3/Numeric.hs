module Numeric(fromRat,
               showSigned, showInt,
               readSigned, readInt,
               readDec, readOct, readHex,
               floatToDigits,
               showEFloat, showFFloat, showGFloat,
               showFloat,
               readFloat, lexDigits) where

import Array
import Ratio
import Char
import Numeric__readSigned
import Numeric__readInt
import Numeric__readFloat
import Numeric__showSigned
import Numeric__showInt
import Numeric__showFloat
import Numeric__formatRealFloat
import Numeric__fromRat


-- Misc utilities to show integers and floats 

showEFloat     :: (RealFloat a) => Maybe Int -> a -> ShowS
showFFloat     :: (RealFloat a) => Maybe Int -> a -> ShowS
showGFloat     :: (RealFloat a) => Maybe Int -> a -> ShowS

showEFloat d x =  showString (formatRealFloat FFExponent d x)
showFFloat d x =  showString (formatRealFloat FFFixed d x)
showGFloat d x =  showString (formatRealFloat FFGeneric d x)

