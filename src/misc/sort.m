/*
**	sort:		Sort a list.  Sorting is stable.
**			Has a few tricks to make it stable and avoid
**			concatenation.
*/
module
export sort, groupsort;
rec sort lt l = qsort lt l []
-- qsort is stable and does not concatenate.
and qsort lt [] r = r
||  qsort lt [x] r = x.r
||  qsort lt (x.xs) r = qpart lt x xs [] [] r
-- qpart partitions and sorts the sublists
and qpart lt x [] rlt rge r =
    -- rlt and rge are in reverse order and must be sorted with an
    -- anti-stable sorting
    rqsort lt rlt (x.rqsort lt rge r)
||  qpart lt x (y.ys) rlt rge r =
    if lt y x then
	qpart lt x ys (y.rlt) rge r
    else
	qpart lt x ys rlt (y.rge) r
-- rqsort is as qsort but anti-stable, i.e. reverses equal elements
and rqsort lt [] r = r
||  rqsort lt [x] r = x.r
||  rqsort lt (x.xs) r = rqpart lt x xs [] [] r
and rqpart lt x [] rle rgt r =
    qsort lt rle (x.qsort lt rgt r)
||  rqpart lt x (y.ys) rle rgt r =
    if lt x y then
	rqpart lt x ys rle (y.rgt) r
    else
	rqpart lt x ys (y.rle) rgt r

-------------
#if 1
-- Sort and group equal element into sublists.
and groupsort lt l = qgsort lt l []
--- qgsort is stable and does not concatenate.
and qgsort lt [] r = r
||  qgsort lt [x] r = grp lt x r
||  qgsort lt (x.xs) r = qgpart lt x xs [] [] r
--- qgpart partitions and sorts the sublists
and qgpart lt x [] rlt rge r =
    -- rlt and rge are in reverse order and must be sorted with an
    -- anti-stable sorting
    rqgsort lt rlt (grp lt x (rqgsort lt rge r))
||  qgpart lt x (y.ys) rlt rge r =
    if lt y x then
	qgpart lt x ys (y.rlt) rge r
    else
	qgpart lt x ys rlt (y.rge) r
-- rqsort is as qsort but anti-stable, i.e. reverses equal elements
and rqgsort lt [] r = r
||  rqgsort lt [x] r = grp lt x r
||  rqgsort lt (x.xs) r = rqgpart lt x xs [] [] r
and rqgpart lt x [] rle rgt r =
    qgsort lt rle (grp lt x (qgsort lt rgt r))
||  rqgpart lt x (y.ys) rle rgt r =
    if lt x y then
	rqgpart lt x ys rle (y.rgt) r
    else
	rqgpart lt x ys (y.rle) rgt r
and grp lt x [] = [[x]]
||  grp lt x (oyys as (ys as (y._)).yys) = 
    if lt x y then ([x].oyys)
    else ((x.ys).yys)
#else
and groupsort lt l = xgroup lt (sort lt l)
and xgroup lt [] = []
||  xgroup lt (x.xs) = xget lt x [] xs
and xget lt x r [] = [x.r]
||  xget lt x r (y.ys) = 
    if lt x y then 
	(x.r).xgroup lt (y.ys)
    else 
	xget lt x (r@[y]) ys
#endif
end
