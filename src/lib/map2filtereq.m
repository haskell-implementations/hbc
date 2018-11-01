/*
**	map2filtereq:	Used by the alternative Array.M for filtering out all
**			elements given an index and the index,value list
**
**			map2filtereq i l = map snd (filter ( \(j,v) . i=j ) l)
*/
module
export Pmap2filtereq;
rec 
   Pmap2filtereq i [] = []
|| Pmap2filtereq i ((j,v).r) = if (i:_Int)=j then v.Pmap2filtereq i r
                                             else   Pmap2filtereq i r
end
