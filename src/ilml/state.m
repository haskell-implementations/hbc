module
#include "../expr/id.t"

export Symbol, st_env, st_lib, st_u, st_def, st_mk, prsymbol, st_menv,
    st_map_menv, st_lds, addlds;

rec type State = S Renv [Symbol Id Cexpr] Int (List Ttype) [String#Renv] [String]

and type Symbol *s *a = Internal *s *a + DynLib String Handle

and prsymbol (Internal i _) = prid i
||  prsymbol (DynLib s _) = "DynLib "@s

and st_env  (S env _   _ _ _ _) = env
and st_lib  (S _   lib _ _ _ _) = lib
and st_u    (S _   _   u _ _ _) = u
and st_def  (S _   _   _ d _ _) = d
and st_menv (S _   _   _ _ m _) = m
and st_lds  (S _   _   _ _ _ l) = l
and st_mk  env lib u dfl menv ls = S env lib u dfl menv ls

and addlds lds (S env lib u d m lds') = S env lib u d m (lds@lds')

and st_map_menv f (S env lib u d m ls) = S env lib u d (f m) ls

end
