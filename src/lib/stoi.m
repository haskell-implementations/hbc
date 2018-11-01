/*
**	stoi:		convert a string to an integer.
*/
module
#include "revitlist.t"
#include "take.t"
#include "isdigit.t"
export	stoi;
rec
   stoi1 (c.cs) r & ('0' <= c & c <= '9') = stoi1 cs (r * 10 + (ord c - ord '0'))
|| stoi1 _ r = r
and
   stoi ('-'.l) = - (stoi1 l 0)
|| stoi l = stoi1 l 0
end
