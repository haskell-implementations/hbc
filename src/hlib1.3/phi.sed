s/;$//
s/{$//
s/^ *}$//
/^interface\(.*\)where.*/d
s/{-# FROMMODULE [^ ]* #-}//
s/{-# IMPORTING [^ ]* #-}//
/infix/d
/data.*Array/d
/data.*Assoc/d
/data.*Complex/d
/data.*Ratio/d
/type.*Rational/d
/^import .*/d
/^instance .*/d
s/Prelude\.//g
