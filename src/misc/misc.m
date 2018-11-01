module -- misc
#include "sort.t"
export Uconcmap, Umap, Uap, Umapsnd, Umapthd, mapthd, butlast, thd, Umap2, split3, split4, Umap3, Umap4, oapply, gsome, findf,
       mixmap, getdups, ynsplit, mapfilter, mapfiltersnd, testconc,
oktestconc, isno, getyes, map3, map4, fstof4, fstof3, sndof4, reduce1,
reduce2, cons2, concmap2;

rec
    Uap f (a, u) = (f a, u)
and
    Umap f []    os = ([], os)
||  Umap f (h.t) os = let (el, ns) = f h os in
		  let (ell, nns) = Umap f t ns in
		  (el.ell, nns)
and
    Umap2 f (h.t) (g.s) os = let (el, ns) = f h g os in
			      let (ell, nns) = Umap2 f t s ns in
			      (el.ell, nns)
 || Umap2 f _ _    os = ([], os)
and
    Umap3 f (h.t) (g.s) (x.y) os = let (el, ns) = f h g x os in
			      let (ell, nns) = Umap3 f t s y ns in
			      (el.ell, nns)
 || Umap3 f _ _ _   os = ([], os)
and
    Umap4 f (h.t) (g.s) (x.y) (z.w) os = let (el, ns) = f h g x z os in
			      let (ell, nns) = Umap4 f t s y w ns in
			      (el.ell, nns)
 || Umap4 f _ _ _ _  os = ([], os)
and
    Uconcmap f []    os = ([], os)
||  Uconcmap f (h.t) os = let (el, ns) = f h os in
			 let (ell, nns) = Uconcmap f t ns in
			 (el@ell, nns)
and
    Umapsnd f []    u = ([], u)
||  Umapsnd f ((a,b).abs) u = let (b', u') = f b u in
			 let (abs', u'') = Umapsnd f abs u' in
			 ((a, b').abs', u'')
and
    Umapthd f []    u = ([], u)
||  Umapthd f ((a,b,c).t) u = let (el, ns) = f c u in
			 let (ell, nns) = Umapthd f t ns in
			 ((a, b, el).ell, nns)
and
    mapthd f [] = []
||  mapthd f ((x,y,z).l) = (x,y,f z).mapthd f l
and butlast l = reverse (tl (reverse l))
and thd (_,_,x) = x
and split3 [] = ([],[],[])
||  split3 ((x,y,z).ws) =
    let (xs, ys, zs) = split3 ws in
    (x.xs, y.ys, z.zs)
and split4 [] = ([],[],[],[])
||  split4 ((x,y,z,v).ws) =
    let (xs, ys, zs, vs) = split4 ws in
    (x.xs, y.ys, z.zs, v.vs)

and oapply f None = None
||  oapply f (Some x) = Some (f x)
and gsome None = []
||  gsome (Some x) = x

and findf []     eq f d = d
||  findf (x.xs) eq f d & (eq x) = f x
||  findf (_.xs) eq f d = findf xs eq f d 

and mixmap f l d = mix (map f l) d

and getdups lt = map hd o filter (\is.length is > 1) o groupsort lt 

and ynsplit [] ys ns = (ys, ns)
||  ynsplit (Yes y.yns) ys ns = ynsplit yns (y.ys) ns
||  ynsplit (No n.yns)  ys ns = ynsplit yns ys (n.ns)

and mapfilter f [] = []
||  mapfilter f (x.xs) =
    case f x in
	None : mapfilter f xs
    ||  Some y : y . mapfilter f xs
    end

and mapfiltersnd f [] = []
||  mapfiltersnd f ((a,b).abs) =
    case f b in
	None : mapfiltersnd f abs
    ||  Some y : (a,y) . mapfiltersnd f abs
    end

and testconc (None._) = None
||  testconc (Some xs.os) =
    case testconc os in
	None : None
    ||  Some ys : Some (xs@ys)
    end
||  testconc [] = Some []

and oktestconc ((n as No _)._) = n
||  oktestconc (Yes xs.os) =
    case oktestconc os in
	(n as No _) : n
    ||  Yes ys : Yes (xs@ys)
    end
||  oktestconc [] = Yes []

and isno (No _) = true
||  isno _ = false
and getyes (Yes y) = y

and map3 f (x1.l1) (x2.l2) (x3.l3) = f x1 x2 x3 . map3 f l1 l2 l3
||  map3 f _ _ _ = []

and map4 f (x1.l1) (x2.l2) (x3.l3) (x4.l4) = f x1 x2 x3 x4 . map4 f l1 l2 l3 l4
||  map4 f _ _ _ _ = []

and show_option f None = "None"
||  show_option f (Some x) = "(Some "@f x@")"

and fstof3 (a,_,_) = a

and fstof4 (a,_,_,_) = a

and sndof4 (_,a,_,_) = a

and cons2 x (a,xs) = (a,x.xs)

and reduce2 f u [] [] = u
||  reduce2 f u (x.xs) (y.ys) = f x y (reduce2 f u xs ys)

and reduce1 f [x] = x
||  reduce1 f (x.xs) = f x (reduce1 f xs)

and concmap2 f (x.xs) (y.ys) = f x y @ concmap2 f xs ys
||  concmap2 _ _ _ = []
end
