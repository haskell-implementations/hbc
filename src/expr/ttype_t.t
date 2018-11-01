import type Kind = mkkarrow Kind Kind + mkkground + mkkvar Int;
import type Context == (List Assert);
import type TyVar == Int;
import type Assert = mkassert Id (List TyVar) + mkaerror (List Char);
import type IDecl = mkidecl Context Id (List Ttype) String;
import type CDecl = mkcdecl Context Assert;
import type Ttype = mktcons Id (List Ttype) + mktvar TyVar + mkterror (List Char) + mktcontype Context Ttype + mktap TyVar (List Ttype);
