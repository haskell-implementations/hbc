module
export qstring;
rec qstring cs = concmap qchar cs
and qchar '\\' = "\\\\"
#ifndef mc68000			       /* \n and \t not available on SUN3 */
||  qchar '\n' = "\\n"
||  qchar '\t' = "\\t"
#endif
||  qchar '"'  = "\\\""
||  qchar c & (' ' <= c & c < chr 127) = [c]
||  qchar c =
    let n = ord c in
    ['\\'; chr (n / 64 + ord '0'); chr ((n/8)%8 + ord '0'); chr (n%8 + ord '0')]
end
