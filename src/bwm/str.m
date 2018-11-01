let rec type Nat = Zero + Succ Nat in
let rec cnc [] ys = ys
||      cnc (x.xs) ys = x.cnc xs ys
in
let rec rep Zero _ = []
||      rep (Succ n) s = cnc s (rep n s)
in
rep (Succ (Succ (Succ Zero))) "ab"
