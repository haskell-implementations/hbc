module

#include "../lib/dialog.t"
#include "../misc/ioc.t"

infixr "@@";
infixr "$";

export topprt,@@,$,sigact;

rec 
    topprt :: Dialog -> Dialog -> Dialog
and topprt d c = 
  let rec
      intc = \e."Interrupt"@@c
  and errc = \e.("Error: "@e@"")@@c
  in sigact excInterrupt (SACatch intc) $
     sigact excError (SACatch errc) $
     execute d c
and execute d c resps = topp (d resps) c resps
and topp reqs c resps =
    let continue msg = ([TOSTDOUT;CCOOKED;CHIATON;chr (-1)] @@ msg @@ c) resps
    and next reqs = case resps in
		  _.resps : topp reqs c resps
	       end
    in case reqs in
	   [] : continue ""
        || Exit 0._ : continue ""
	|| Exit n._ : continue ("Exit " @ itos n)
	    -- We are liberal to SigActions and might loose control, 
	    -- e.g. if the user does SAIgnore on interrupt.
	|| SigAction exc (SACatch h).reqs :
	    SigAction exc (SACatch (\e.execute (h e) c)).next reqs
	|| r.reqs : r.next reqs
	end

and (@@) :: String -> Dialog -> Dialog
and s @@ d = appendChan stdout s exit d
and a $ b = a b

and sigact exc act c = sigAction exc act exit (\_.c)

end
