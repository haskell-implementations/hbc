let rec
    type Tree = Empty + Node Tree Tree
and type Nat = Zero + Succ Nat
and mirror Empty = Empty
||  mirror (Node t1 t2) = Node (mirror t2) (mirror t1)
and eqt Empty Empty = true
||  eqt (Node t11 t12) (Node t21 t22) = eqt t11 t21 & eqt t12 t22
||  eqt _ _ = false
and build Zero = Empty
||  build (Succ n) = Node (build n) (build n)
and n16 = Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero)))))))))))))))
in
let t = mirror (build n16) in
if eqt t t then "t\n" else "f\n"
/*
dmirror -S -H8000000 -h8000000
   Heap   Stk    GC(real) GC acc (real)     tot (real) newheap
         0 GCs,
      4.74 (5.7) seconds total time,
      0.00 (0.0) seconds GC time ( 0.0( 0.0)% of total time)
   4981272 bytes allocated from the heap.
2 chunks allocated
No of CONC reductions: 0, of which 0 used associativity
No of bigcmp: 0, eq: 0, ind: 0
*/

