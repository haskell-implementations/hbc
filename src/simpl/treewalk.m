module
#include "../ExprE/Expr_t.t"
#include "../ExprE/Eprint.t"
#include "../misc/misc.t"
export treewalk;
rec
    treewalk f e =
    case e in
        Ecase et cies de : f e (Ecase (treewalk f et) (mapthd (treewalk f) cies) (treewalk f de))
    ||  Econstr c es : f e (Econstr c (map (treewalk f) es))
    ||  Elet r ds ed : f e (Elet r (mapsnd (treewalk f) ds) (treewalk f ed))
    ||  Eidapl i es : f e (Eidapl i (map (treewalk f) es))
    ||  Elaml is eb : f e (Elaml is (treewalk f eb))
    ||  Efailmatch n : f e e
    ||  Ecfunction _ _ : f e e
    ||  Einfo i eb : f e (Einfo i (treewalk f eb))
    ||  _ : fail ("treewalk:"@pr e)
    end
end
