module TPL.Pattern where

import TPL.Value

unify :: [TPLValue] -> [TPLValue] -> [(String, TPLValue)]
unify exps vals = let exps' = map squash exps
                      vals' = map squash vals ++ repeat Null in
                  zip exps' vals' >>= unifyExp
  where unifyExp ((Id name), val)          = [(name, val)]
        unifyExp ((List ls), (List val))   = case last ls of
          Expression [pattern, Operator "..."] -> unify (init ls) (take len val) ++ unifyRest pattern (drop len val)
          _                                    -> unify ls val
          where len = length $ init ls
        unifyExp (ls@(List _), val)        = unifyExp (ls, List [val])
        unifyExp (Lambda _ exp, val)       = unifyExp (exp, val)
        unifyRest rest []   = unify [rest] [Null]
        unifyRest rest vals = combine $ vals >>= \ val -> unify [rest] [val]
        combine ((name, val):rest) = (name, List $ val : extract name rest) : 
                                     (combine $ filter ((/= name) . fst) rest)
        combine []                 = []
        extract name ls = [v | (n, v) <- ls, n == name]
          
squash :: TPLValue -> TPLValue
squash (Expression [val]) = squash val
squash (Sequence [val])   = squash val
squash val                = val