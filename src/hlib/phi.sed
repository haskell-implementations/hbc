s/;$//
s/{$//
s/^ *}$//
s/^interface\(.*\)where.*/--- \1/
s/{-# FROMMODULE [^ ]* #-}//
/{-# IMPORTING/d
/infix/d
/data.*Array/d
/data.*Assoc/d
/data.*Complex/d
/data.*Ratio/d
/type.*Rational/d
/^import .*/d
