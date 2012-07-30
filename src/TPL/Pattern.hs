module TPL.Pattern where

import TPL.Value

unify :: [Term] -> [Value] -> Env
unify pats vals = let vals' = vals ++ repeat Null in
  zip pats vals' >>= unifyPat
  where unifyPat (Id name, val) = [(name, val)]
        unifyPat (ListLiteral ls, List vs) = case last ls of
          Rest pat -> unify (init ls) (take len vs) ++ unifyRest pat (drop len vs)
          _        -> unify ls vs
          where len = length $ init ls
        unifyPat (ls@ListLiteral{}, val) = unifyPat (ls, List [val])
        unifyPat (Lambda _ pat, val) = unifyPat (pat, val)
        unifyPat (pat, _) = error $ "Invalid pattern: " ++ show pat
        
        unifyRest rest []   = unify [rest] []
        unifyRest rest restVals = combine $ restVals >>= \ val -> unify [rest] [val]
        
        combine ((name, val):rest) = (name, List $ val : [v | (n, v) <- rest, n == name]) :
                                            (combine $ filter ((/= name) . fst) rest)
        combine []                 = []