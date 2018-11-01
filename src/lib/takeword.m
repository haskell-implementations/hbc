/*
**	takeword:
**		This function takes a string, and returns a pair of strings,
**		the first component being the first logical word of the original
**		string, the second component being the rest of the string.
**		When extracting a word, the common "all purpose standard" is
**		followed:
**		
**		  1. A sequence of one or more spaces, newlines or tabs are
**		       skipped over, being considered as a word separator.
**		  2. A sequence beginning with a letter or '_', followed by zero
**		       or more letters, digits or '_'s, is considered one word.
**		  3. A sequence of one or more digits is one word.
**		  4. All other characters are considered as separate words.
**		
**		When no word is found in the string, two empty strings are
**		returned.
**		
**		Ex: takeword "a_b c d" = "a_b"," c d"
**		    takeword " 35_x a" = "35","_x a"
**		    takeword "\n\t (* comment *)" = "(","* comment *)"
**		    takeword "  \n  \t\t " = "",""
**		    choplist takeword
**		      "let b1 = pred(x) in\nb1 => x*10.3 | x div 10" =
**		      ["let"; "b1"; "="; "pred"; "("; "x"; ")"; "in"; "b1"; "=";
**		       ">"; "x"; "*"; "10"; "."; "3"; "|"; "x"; "div"; "10"; ""]
*/

module
-- WARNING: not self contained

#include "isalpha.t"
#include "isalnum.t"
#include "isdigit.t"
#include "take.t"

  export takeword;
  rec

  takeword [] = [],[]
  ||
  takeword (' '.l) = takeword l
  ||
  takeword ('\n'.l) = takeword l
  ||
  takeword ('\t'.l) = takeword l
  ||
  takeword (x.l) =
    if isalpha x | x = '_'
      then
        let (l1,l2) = take (\c.isalnum c | c='_') l
        in  x.l1,l2
      else
    if isdigit x
      then
        let (l1,l2) = take (\d.isdigit d) l
        in  x.l1,l2
      else [x],l

end
