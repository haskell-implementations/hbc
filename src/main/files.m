module
#include "../misc/flags.t"
infix ";;;";
export finput, ftype, fotype, fasm, fgcode, fbwm, fstdout, fstderr, modulename, groupname;
rec fstderr = [TOSTDERR]
and fstdout = [TOSTDOUT]
and read f = 
    case openfile f in
	Yes s : s
    ||  No s : fail s @ f
    end
and (finput0, ftype, fotype, fasm, fgcode, fbwm) =
	case basename in
	   No msg : fail msg
	|| Yes "" : (read "/dev/fd0", fstdout, fstdout, fstdout, fstdout, fstdout)
	|| Yes n : (read (n@".p"), stofile (n@".t"), stofile (n@".ot"), stofile (n@".s"), stofile (n@".g"), stofile (n@".bmc"))
	end
and x ;;; y = seq x y
-- a hack to avoid a space leak in the tuple selection.
and finput = ftype ;;; fotype ;;; fasm ;;; fgcode ;;; fbwm ;;; finput0

and basename2 xs = 
	let rec ubname b1 b2 [] = (reverse b1, reverse b2)
	     || ubname b1 b2 ('/'.cs) = ubname b2 [] cs
             || ubname b1 b2 (c.cs) = ubname b1 (c.b2) cs
	in
	    ubname [] [] xs

and pathname = getwd @ "/" @ realname
and (groupname, filename) = basename2 pathname
and modulename = fst (splitat '.' filename)
end
