module
--
-- FLIC reader
--
#include "Flic_t.t"
export fread;
rec
    skipsp (cl as c.l) = if isspace c then skipsp l else cl
||  skipsp [] = []
and rlist f l =
	case skipsp l in
	   ')'.l : ([], l)
	|| l :	let! (x, l) = f l in
		let! (xs, l) = rlist f l in
		(x.xs, l)
	end
and ir l = let (i, l) = take isdigit l in (Fnumber (stoi i), l)
and E1 l =
	case skipsp l in
	   [] : (Ffail, [])
	|| (cl as c.l) :
    	case c in
	   '(' :	let! (f, l) = E l in
			let! (')'.l) = skipsp l in
			(f, l)
	|| '"' :	let! (n, l) = nr cl in
			(Fname n, l)
	|| '#' :	ir l
	|| '\'':	cr l
	|| _   :	(Ffail, l)
	end
	end
and E' l =
	case skipsp l in
	   [] : (Ffail, [])
	|| (cl as c.l) :
    	case c in
	   '\\' :	let! (n, l) = nr l in
			let! (f, l) = E l in
			(Flam n f, l)
	|| '=' :	case skipsp l in
			   '('.l :	let! (ns, l) = rlist nr l in
					let! ('('.l) = skipsp l in
					let! (fs, l) = rlist E1 l in
					let! (f, l) = E l in
					(Flet false ns fs f, l)
			|| l :		let! (n, l) = nr l in
					let! (e, l) = E1 l in
					let! (f, l) = E l in
					(Flet false [n] [e] f, l)
			end
	|| '&' :	let! ('('.l) = skipsp l in
			let! (ns, l) = rlist nr l in
			let! ('('.l) = skipsp l in
			let! (fs, l) = rlist E1 l in
			let! (f, l) = E l in
			(Flet true ns fs f, l)
	|| '[' :	let! (a, l) = ar l in
			let! (f, l) = E l in
			(Fannot a f, l)
	|| _   :	E1 cl
	end
	end
and E l =
	(let! (f, l) = E' l in
	g f l
	    where rec g f l =
			case E1 l in
			   (Ffail, _) : (f, l)
			|| (a, l) : g (Fap f a) l
			end)
and ar ll = 
	let (n, l) = nr (skipsp ll) in
	case E l in
	   (Ffail, _) : (Annot0 n, snd (splitat ']' ll))
	|| (f, l) : (Annot1 n f, snd (splitat ']' ll))
	end
and nr l = let! ('"'.l) = skipsp l in splitat '"' l
-- add handling of escapes
and cr l = (cr' (skipsp l) where
    cr' ('"'.l) = let (s, l) = splitat '"' l in (Fstring s, l)
||  cr' (c.l) = (Fchar c, l))
and fread l = fst (E l)
end
