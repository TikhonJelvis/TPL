module TPL.Pattern (unify) where

import           TPL.Value

unify :: Term -> Value -> [(String, Value)]
unify (Expression [term]) val    = unify term val
unify (Id name) val              = [(name, val)]
unify (ListLiteral ls) (List vs) = case last ls of
  Expression [pat, Operator "..."] ->
    unifyMany (init ls) (take len vs) ++ unifyRest pat (drop len vs)
  _ -> unifyMany ls vs
  where len                      = length $ init ls
unify (ls@ListLiteral{}) val     = unify ls $ List [val]
unify (Lambda _ pat) val         = unify pat val
unify pat _                      = error $ "Invalid pattern: " ++ show pat

unifyMany :: [Term] -> [Value] -> [(String, Value)]
unifyMany pats vals = zip pats (vals ++ repeat Null) >>= uncurry unify

unifyRest :: Term -> [Value] -> [(String, Value)]
unifyRest pat []       = unify pat Null
unifyRest pat restVals = combine $ restVals >>= unify pat
  where combine ((name, val):rest) = (name, List $ val : [v | (n, v) <- rest, n == name]) :
                             (combine $ filter ((/= name) . fst) rest)
        combine []                 = []
