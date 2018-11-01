/*
**	stoiI:		convert a string to an Integer.
*/
module
export	stoiI;
rec
--   stoi1 l = revitlist (\x.\y.Int2Integer (ord x - ord '0') +# 10# *# y) (fst(take isdigit l)) 0#
   stoi1 (c.cs) r & ('0' <= c & c <= '9') = stoi1 cs (r *# 10# +# Int2Integer (ord c - ord '0'))
|| stoi1 _ r = r
and
   stoiI ('-'.l) = 0# -# (stoi1 l 0#)
|| stoiI l = stoi1 l 0#
end
