module PreludeX where

_mapFst f xys = [ (f x, y) | (x, y) <- xys ]

_readRec :: [(String, String -> [(a->a, String)])] -> a -> String -> ReadS a
_readRec lfs r con s0 =
    [ (r, s3)  | (con', s1) <- lex s0, con' == con,
                 ("{" , s2) <- lex s1,
                 (r   , s3) <- rField r s2
    ]
  where rField r s0 =
    	    [ (r,  s1) | ("}", s1) <- lex s0 ]
            ++
            [ (r', s4) | (tok, s1) <- lex s0,
                         (l  ,  f) <- lfs, l==tok,
                         ("=", s2) <- lex s1,
                         (upd, s3) <- f s2,
                         (r' , s4) <- rField' (upd r) s3
            ]
        rField' r s0 =
            [ (r,  s1) | ("}", s1) <- lex s0]
            ++
            [ (r', s2) | (",", s1) <- lex s0,
                         (r' , s2) <- rField r s1]
