module -- Tree234
#include "utils.t"

export treeMap, treeCombine, treeRebuild, treeFromList, treeMapList,
               treeList, treeUpdate, treeSearch, treeAdd, initTree234;

rec
type Tree234 *a = Leaf +
                 Leaf2 *a +
                 Leaf3 *a *a +
                 Leaf4 *a *a *a +
                 Node2 *a (Tree234 *a) (Tree234 *a) +
                 Node3 *a *a (Tree234 *a) (Tree234 *a) (Tree234 *a) +
                 Node4 *a *a *a (Tree234 *a) (Tree234 *a) (Tree234 *a) (Tree234 *a) 

and initTree234 = Leaf

and treeAdd comb cmp a t = treeAdd' comb cmp a id Node2 t

and treeSearch fail cont p Leaf = fail
||  treeSearch fail cont p (Leaf2 a1) = p a1 fail (cont a1) fail
||  treeSearch fail cont p (Leaf3 a1 a2) =
    p a1 fail (cont a1) (p a2 fail (cont a2) fail)
||  treeSearch fail cont p (Leaf4 a1 a2 a3) =
    p a2 (p a1 fail (cont a1) fail) (cont a2) (p a3 fail (cont a3) fail)
||  treeSearch fail cont p (Node2 a1 t1 t2) =
    p a1 (treeSearch fail cont p t1) (cont a1) (treeSearch fail cont p t2)
||  treeSearch fail cont p (Node3 a1 a2 t1 t2 t3) =
    p a1
      (treeSearch fail cont p t1)
      (cont a1)
      (p a2
         (treeSearch fail cont p t2)
         (cont a2)
         (treeSearch fail cont p t3))
||  treeSearch fail cont p (Node4 a1 a2 a3 t1 t2 t3 t4) =
    p a2
      (p a1
         (treeSearch fail cont p t1)
         (cont a1)
         (treeSearch fail cont p t2))
      (cont a2)
      (p a3
         (treeSearch fail cont p t3)
         (cont a3)
         (treeSearch fail cont p t4))

and treeUpdateFailed = fail "treeUpdate couldn't find the node"

and treeUpdate update p Leaf = Leaf
||  treeUpdate update p (Leaf2 a1) =
    p a1 treeUpdateFailed (Leaf2 (update a1)) treeUpdateFailed
||  treeUpdate update p (Leaf3 a1 a2) =
    p a1
      treeUpdateFailed
      (Leaf3 (update a1) a2)
      (p a2 treeUpdateFailed (Leaf3 a1 (update a2)) treeUpdateFailed)
||  treeUpdate update p (Leaf4 a1 a2 a3) =
    p a2
      (p a1 treeUpdateFailed (Leaf4 (update a1) a2 a3) treeUpdateFailed)
      (Leaf4 a1 (update a2) a3)
      (p a3 treeUpdateFailed (Leaf4 a1 a2 (update a3)) treeUpdateFailed)
||  treeUpdate update p (Node2 a1 t1 t2) =
    p a1
      (Node2 a1 (treeUpdate update p t1) t2)
      (Node2 (update a1) t1 t2)
      (Node2 a1 t1 (treeUpdate update p t2))
||  treeUpdate update p (Node3 a1 a2 t1 t2 t3) =
    p a1
      (Node3 a1 a2 (treeUpdate update p t1) t2 t3)
      (Node3 (update a1) a2 t1 t2 t3)
      (p a2
         (Node3 a1 a2 t1 (treeUpdate update p t2) t3)
         (Node3 a1 (update a2) t1 t2 t3)
         (Node3 a1 a2 t1 t2 (treeUpdate update p t3)))
||  treeUpdate update p (Node4 a1 a2 a3 t1 t2 t3 t4) =
    p a2
      (p a1
         (Node4 a1 a2 a3 (treeUpdate update p t1) t2 t3 t4)
         (Node4 (update a1) a2 a3 t1 t2 t3 t4)
         (Node4 a1 a2 a3 t1 (treeUpdate update p t2) t3 t4))
      (Node4 a1 (update a2) a3 t1 t2 t3 t4)
      (p a3
         (Node4 a1 a2 a3 t1 t2 (treeUpdate update p t3) t4)
         (Node4 a1 a2 (update a3) t1 t2 t3 t4)
         (Node4 a1 a2 a3 t1 t2 t3 (treeUpdate update p t4)))

and treeMapList f Leaf = []
||  treeMapList f (Leaf2 a1) = f a1
||  treeMapList f (Leaf3 a1 a2) = f a1 @ f a2
||  treeMapList f (Leaf4 a1 a2 a3) = f a1 @ f a2 @ f a3
||  treeMapList f (Node2 a1 t1 t2) =
    treeMapList f t1 @ f a1 @ treeMapList f t2
||  treeMapList f (Node3 a1 a2 t1 t2 t3) =
    treeMapList f t1 @
    f a1 @ treeMapList f t2 @ f a2 @ treeMapList f t3
||  treeMapList f (Node4 a1 a2 a3 t1 t2 t3 t4) =
    treeMapList f t1 @
    f a1 @
    treeMapList f t2 @
    f a2 @ treeMapList f t3 @ f a3 @ treeMapList f t4

and treeMap f Leaf = Leaf
||  treeMap f (Leaf2 a1) = Leaf2 (f a1)
||  treeMap f (Leaf3 a1 a2) = Leaf3 (f a1) (f a2)
||  treeMap f (Leaf4 a1 a2 a3) = Leaf4 (f a1) (f a2) (f a3)
||  treeMap f (Node2 a1 t1 t2) =
    Node2 (f a1) (treeMap f t1) (treeMap f t2)
||  treeMap f (Node3 a1 a2 t1 t2 t3) =
    Node3 (f a1) (f a2) (treeMap f t1) (treeMap f t2) (treeMap f t3)
||  treeMap f (Node4 a1 a2 a3 t1 t2 t3 t4) =
    Node4 (f a1)
          (f a2)
          (f a3)
          (treeMap f t1)
          (treeMap f t2)
          (treeMap f t3)
          (treeMap f t4)

and treeFromList comb cmp l = treeAddList comb cmp l Leaf

and treeAddList comb cmp [] t = t
||  treeAddList comb cmp (x . xs) t =
    treeAddList comb cmp xs (treeAdd comb cmp x t)

and treeRebuild comb cmp t = treeFromList comb cmp (treeMapList (. []) t)

and treeCombine comb cmp t1 t2 =
    treeAddList comb cmp (treeMapList (. []) t2) t1

and treeAdd' comb cmp a keep split Leaf = keep (Leaf2 a)
||  treeAdd' comb cmp a keep split (Leaf2 a1) =
    keep (cmp a a1 (Leaf3 a a1) (Leaf2 (comb a a1)) (Leaf3 a1 a))
||  treeAdd' comb cmp a keep split (Leaf3 a1 a2) =
    keep (cmp a
              a1
              (Leaf4 a a1 a2)
              (Leaf3 (comb a a1) a2)
              (cmp a a2 (Leaf4 a1 a a2) (Leaf3 a1 (comb a a2)) (Leaf4 a1 a2 a)))
||  treeAdd' comb cmp a keep split (Leaf4 a1 a2 a3) =
    cmp a
        a2
        (cmp a
             a1
             (split a2 (Leaf3 a a1) (Leaf2 a3))
             (keep (Leaf4 (comb a a1) a2 a3))
             (split a2 (Leaf3 a1 a) (Leaf2 a3)))
        (keep (Leaf4 a1 (comb a a2) a3))
        (cmp a
             a3
             (split a2 (Leaf2 a1) (Leaf3 a a3))
             (keep (Leaf4 a1 a2 (comb a a3)))
             (split a2 (Leaf2 a1) (Leaf3 a3 a)))
||  treeAdd' comb cmp a keep split (Node2 a1 t1 t2) =
    keep (cmp a
              a1
              (treeAdd' comb
                        cmp
                        a
                        (\t1'. Node2 a1 t1' t2)
                        (\a0'. \t0'. \t1'. Node3 a0' a1 t0' t1' t2)
                        t1)
              (Node2 (comb a a1) t1 t2)
              (treeAdd' comb
                        cmp
                        a
                        (\t2'. Node2 a1 t1 t2')
                        (\a2'. \t2'. \t3'. Node3 a1 a2' t1 t2' t3')
                        t2))
||  treeAdd' comb cmp a keep split (Node3 a1 a2 t1 t2 t3) =
    keep (cmp a
              a1
              (treeAdd' comb
                        cmp
                        a
                        (\t1'. Node3 a1 a2 t1' t2 t3)
                        (\a0'. \t0'. \t1'. Node4 a0' a1 a2 t0' t1' t2 t3)
                        t1)
              (Node3 (comb a a1) a2 t1 t2 t3)
              (cmp a
                   a2
                   (treeAdd' comb
                             cmp
                             a
                             (\t2'. Node3 a1 a2 t1 t2' t3)
                             (\a1_5'.
                              \t1_5'.
                              \t2'. Node4 a1 a1_5' a2 t1 t1_5' t2' t3)
                             t2)
                   (Node3 a1 (comb a a2) t1 t2 t3)
                   (treeAdd' comb
                             cmp
                             a
                             (\t3'. Node3 a1 a2 t1 t2 t3')
                             (\a3'.
                              \t3'. \t4'. Node4 a1 a2 a3' t1 t2 t3' t4')
                             t3)))
||  treeAdd' comb cmp a keep split (Node4 a1 a2 a3 t1 t2 t3 t4) =
    cmp a
        a2
        (cmp a
             a1
             (split a2
                    (treeAdd' comb
                              cmp
                              a
                              (\t1'. Node2 a1 t1' t2)
                              (\a0'. \t0'. \t1'. Node3 a0' a1 t0' t1' t2)
                              t1)
                    (Node2 a3 t3 t4))
             (keep (Node4 (comb a a1) a2 a3 t1 t2 t3 t4))
             (split a2
                    (treeAdd' comb
                              cmp
                              a
                              (\t2'. Node2 a1 t1 t2')
                              (\a2'. \t2'. \t3'. Node3 a1 a2' t1 t2' t3')
                              t2)
                    (Node2 a3 t3 t4)))
        (keep (Node4 a1 (comb a a2) a3 t1 t2 t3 t4))
        (cmp a
             a3
             (split a2
                    (Node2 a1 t1 t2)
                    (treeAdd' comb
                              cmp
                              a
                              (\t3'. Node2 a3 t3' t4)
                              (\a2'. \t2'. \t3'. Node3 a2' a3 t2' t3' t4)
                              t3))
             (keep (Node4 a1 a2 (comb a a3) t1 t2 t3 t4))
             (split a2
                    (Node2 a1 t1 t2)
                    (treeAdd' comb
                              cmp
                              a
                              (\t4'. Node2 a3 t3 t4')
                              (\a4'. \t4'. \t5'. Node3 a3 a4' t3 t4' t5')
                              t4)))

and treeList t = treeMapList (. []) t

end
