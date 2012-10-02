module TPL.Pattern (unify) where

import           TPL.Value

  -- TODO: fix how delayed things are matched!
unify :: Term -> Value -> [(Value, Value)]
unify (Expression _ [pat]) val     = unify pat val
unify (Lambda _ pat) val          = unify pat val
unify (Id name) val               = [(String name, val)]
unify (ListLiteral ls) (List vs)  = case last ls of
  Expression _ [pat, Operator "..."] ->
    unifyMany (init ls) (take len vs) ++ unifyRest pat (drop len vs)
  _ -> unifyMany ls vs
  where len = length $ init ls
unify (ls@ListLiteral{}) val      = unify ls $ List [val]
unify pat _                       = error $ "Invalid pattern: " ++ show pat

unifyMany :: [Term] -> [Value] -> [(Value, Value)]
unifyMany pats vals = zip pats (vals ++ repeat Null) >>= uncurry unify

unifyRest :: Term -> [Value] -> [(Value, Value)]
unifyRest pat []       = unify pat $ List []
unifyRest pat restVals = combine $ restVals >>= unify pat
  where combine ((name, val):rest) = (name, List $ val : [v | (n, v) <- rest, n == name]) :
                             (combine $ filter ((/= name) . fst) rest)
        combine []                 = []
