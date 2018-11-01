/*  Phil Wadler, 15 May 87 */
/*  esc ys x
      convert character x to an appropriate escape sequence;
      escape all special characters, plus characters in ys.  */

module
export esc;
rec
    esc ys x  =  if  mem x ys  then  ['#';x]
		 else if  x = '\n'  then  "#n"
		 else if  x = '\t'  then  "#t"
		 else if  x = '\f'  then  "#f"
		 else if  x < ' ' | x > '~'  then
		   "#x" @ [hex ((ord x/16)%16); hex (ord x%16)]
		 else
		   [x]
and
    hex x           =  if  0 <= x  & x <= 9   then
			 chr (ord '0' + x)
		       else
			 chr (ord 'a' + x - 10)
end
