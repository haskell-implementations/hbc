/*
**	getenv:		get environment variable
*/
module
import envp:List (List Char);		-- defined in the runtime system
export getenv;
--- WARNING: not self-contained
rec
    env = map (\l.let (a,b)=splitat '=' l in (a, Yes b)) envp
and
    getenv var = assocdef var env (No "")
end
