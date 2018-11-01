module
#include "id_t.t"
#include "ttype_t.t"
#include "types_t.t"
#include "einfo_t.t"
-- Parse annotations.
-- Some annotations are dangerous (STRICT, NOEVAL) use them
-- at your own risk.
export parse_annot;
rec
    parse_annot s = parse (words s)
and parse ("SPARK".is) = spark (map (\s.mkids ('_'.s)) is)
||  parse ["STRICT"] = strict
||  parse ["NOEVAL"] = noeval
||  parse ["INLINE"] = inline
--||  parse ["NOARROW"] = noarrow
||  parse ["OVERLOAD"] = overload
||  parse ["NOTCONST"] = notconst
||  parse ["VECTORDEF"] = vectordef
||  parse ["VECREG2"] = vecreg2
||  parse ["LIMITOK"] = limitok
||  parse ["METCALL"] = metcall
||  parse ["NOTCHK"] = notchk
||  parse ["POSITION";line;file] = position file (stoi line)
||  parse a = fail ("Bad annotation: " @ mix a " ")
and words (' '.s) = words s
||  words s = case s in
		"" : []
	      || s' : let (w,ss) = splitat ' ' s' in w . words ss
	      end
end
