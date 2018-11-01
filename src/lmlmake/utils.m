module

infix "oo";

--export afst, asnd, apair, aboth, pairwith, swap, anth, dropto, number,
--K, I, C, gmap,unionmap, remove, lhead, ltail, part;

    afst f (x,y)=(f x,y)
and asnd f (x,y)=(x,f y)
and aboth f (x,y)=(f x,f y)
and apair (f,g) (x,y)=(f x,g y)
and pairwith f x=(x,f x)
and swap (x,y) = (y,x)

and (f oo g) x y = f (g x y)

and rec anth _ _ [] = []
     || anth 1 f (x.xs) = f x.xs
     || anth n f (x.xs) = x.anth (n-1) f xs

and rec dropto p=while (\l.l~=[] & (not o p o hd) l) tl

and number _ [] = []
 || number i (x.xs) = (i,x).number (i+1) xs

and const x y=x
and id x=x
and C f x y=f y x
and loop f = let rec yf = f yf in yf

	-- gmap g f = reduce g [] o map f
and rec gmap g f = reduce (\x.\ys.g (f x) ys) []
and unionmap f = gmap union f

and remove a (b.bs) & (a = b) = bs
 || remove a (b.bs) = b.remove a bs
 || remove a [] = []

and replace p [] = [p]
||  replace (t,v) ((t',v').ls) & (t = t') = ((t,v).ls)
||  replace p (l.ls) = l.replace p ls

-- lhead xs ys = head (length xs) ys, but the rhs is stricter
and lhead (x.xs) (y.ys) = y.lhead xs ys
 || lhead _ _ = []

-- ltail xs ys = tail (length xs) ys, but the rhs is stricter
and ltail [] ys = ys
 || ltail _ [] = []
 || ltail (x.xs) (y.ys) = ltail xs ys

-- lsplit xs ys = (lhead xs ys,ltail xs ys), but without the space leak
and lsplit [] ys = ([],ys)
 || lsplit _  [] = ([],[])
 || lsplit (x.xs) (y.ys) =
	let (yhs,yts) = lsplit xs ys
	in (y.yhs,yts)

-- JSP 920928
and part p []     = ([], [])
||  part p (x.xs) = 
    let  (ys, zs) = part p xs in
	if p x then (x.ys, zs) else (ys, x.zs)


and issubset a b = all (C mem b) a

end
