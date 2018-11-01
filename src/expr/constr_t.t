import type ConstructorType = Gint + Gchar + Gstring String + Gtype + Gdfloat String + Ginteger String + Gsfloat String;
import type Tinfo = mktinfo Ttype Int Bool Bool (List Atype) Bool Bool (Option Id);
import type Constr = Cconstr String Ttype Tinfo Int (#3 Bool (List Int) (List Assert)) (List (Ttype # Bool));
