/*
**	subsets:
**		Gives a list of all subsets of a given set (list)
**		
**		subsets [1;2;3] = [[1;2;3];[1;2];[1;3];[1];[2;3];[2];[3];[]]
*/
module
-- WARNING: not self contained
export subsets;
rec
  subsets [] = [[]]
  ||
  subsets (h.t) =
    let
      s=subsets t
    in
      map(\x.(h.x))s @ s
end
