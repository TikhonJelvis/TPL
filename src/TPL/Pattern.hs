module TPL.Pattern (unify, squash) where

import TPL.Value

unify :: [TPLValue] -> [TPLValue] -> [(String, TPLValue)]
unify exprs vals = let exprs' = map squash exprs
                       vals'  = map squash vals ++ repeat Null in
                  zip exprs' vals' >>= unifyExpr
  where unifyExpr ((Id name), val)          = [(name, val)]
        unifyExpr ((List ls), (List val))   = case last ls of
          Expression [pattern, Operator "..."] -> unify (init ls) (take len val) ++ unifyRest pattern (drop len val)
          _                                    -> unify ls val
          where len = length $ init ls
        unifyExpr (ls@(List _), val)        = unifyExpr (ls, List [val])
        unifyExpr (Lambda _ expr, val)       = unifyExpr (expr, val)
        unifyExpr (expr, val)                = unifyExpr (String $ show expr, val)
        unifyRest rest []   = unify [rest] [Null]
        unifyRest rest values = combine $ values >>= \ val -> unify [rest] [val]
        combine ((name, val):rest) = (name, List $ val : extract name rest) : 
                                     (combine $ filter ((/= name) . fst) rest)
        combine []                 = []
        extract name ls = [v | (n, v) <- ls, n == name]
          
squash :: TPLValue -> TPLValue
squash (Expression [val])  = squash val
squash (Sequence [val])    = squash val
squash val                 = val