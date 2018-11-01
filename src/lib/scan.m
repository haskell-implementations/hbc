/*
**	scan:
**	Scan family:	convert a string to a value, return a pair of the
**			  value and the rest of string.
**
**	scan_int st	scan an integer. integers may have one of the forms:
**			  <unsigned int>
**			  {+|-|~} <unsigned int>
**	scan_bool st	scan a boolean. forms:
**			  true  | TRUE  | True  | tt | TT | t | T
**			  false | FALSE | False | ff | FF | f | F
**	scan_char st	scan a char.    forms:
**			  '<char>'
**			   <char>
**	scan_string st	scan a list of chars.  forms:
**			  " {<char>} "
**			  <char list>
**	scan_list f st	scan a list, elements scaned with f. forms:
**			  nil
**			  <element> .  <list>
**			  <element> :: <list>
**			  <element> :  <list>
**			  []
**			  [ <element> {; <element>} ]
**			  [ <element> {, <element>} ]
**	scan_pair (f,g) st  scan a pair, components scaned with f and g. forms:
**			      ( <component> , <component> )
**			      <component> , <component>
**
**			Use include file scan to get these functions.
**			Example: scan a list of pairs, the pair is a boolean
**			and an integer:
**	scan_list (scan_pair (scan_bool, scan_int)) "[(true, 5); (false,-1)]"
**	  = ([(true, 5); (false,-1)],"")
*/
module
-- WARNING: not self contained

export	scan_int, scan_bool, scan_char, scan_string, scan_list, scan_pair;
rec
    scanpar f s = case takeword s in
		    ("(",r): let (v,(")",rr)) = (\(x,y).(x,takeword y)) (f r)
			     in  v,rr
		  end
and
    scan_bool s = let (w,r) = takeword s
		  in  if mem w ["true";"TRUE";"True";"tt";"TT";"t";"T"]
		        then (true,r)
		        else
		      if mem w ["false";"FALSE";"False";"ff";"FF";"f";"F"]
			then (false,r)
			else
		      if w="("
			then scanpar scan_bool s
			else fail "<boolean> expected.\n"
and
    scan_int s = case takeword s in
		   ("+",r): scan_int r                   ||
		   ("-",r): (\(x,y).(-x,y)) (scan_int r) ||
		   ("~",r): (\(x,y).(-x,y)) (scan_int r) ||
		   ("(",_): scanpar scan_int s           ||
		   (n,r): if all isdigit n
			    then (stoi n),r
			    else fail "<integer> expected.\n"
		 end
and
    scan_char s = case snd(take isspace s) in
		    ('''.c.'''.r): (c,r)         ||
		    ('''._): fail "non-matching singel-quote (') in <char>" ||
		    ('('._): scanpar scan_char s ||
		    (c.r): (c,r)
		  end
and
    scan_string s = case snd(take isspace s) in
		      ('"'.s): let (st,'"'.r) = take (~= '"') s
			       in  st,r                         ||
		      ('('._): scanpar scan_string s            ||
		      _: scan_list scan_char s
		    end
and
    scantail f s = case snd(take isspace s) in
		     ('.'.s):     scan_list f s ||
		     (':'.':'.s): scan_list f s ||
		     (':'.s):     scan_list f s ||
		     _ : fail "<cons-op> expected.\n"
		   end
and
    scanrest f s = case takeword s in
		     ("]",s): [],s ||
		     (";",s): let (v,r) = f s in
			      let (vv,rr) = scanrest f r
                              in  (v.vv),rr              ||
		     (",",s): let (v,r) = f s in
			      let (vv,rr) = scanrest f r
                              in  (v.vv),rr              ||
		     (_,_) : fail "<list terminator/separator> expected.\n"
		   end
and
    scan_list f s = case takeword s in
		      ("nil",s): ([],s)                ||
		      ("[",s): case takeword s in
				 ("]",r): ([],r)     ||
				 _: scanrest f (';'.s)
			       end                     ||
		      ("(",_): scanpar (scan_list f) s ||
		      _:  let (v,r) = f s in
			  let (vv,rr)= scantail f r
			  in  ((v.vv),rr)
		    end
and
    scan_pair (f,g) s = case takeword s in
			  ("(",s): let (v1,r) = f s          in
				   let (",",rr) = takeword r in
				   let (v2,rrr) = g rr       in
				   let (")",rrrr) = takeword rrr
				   in  (v1,v2),rrrr              ||
			  (_,_) : let (v1,r) = f s          in
				  let (",",rr) = takeword r in
				  let (v2,rrr) = g rr
				  in  (v1,v2),rrr
		        end
end
