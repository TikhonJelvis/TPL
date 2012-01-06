module TPL.Pattern where

import TPL.Value

unify :: [TPLValue] -> [TPLValue] -> [(String, TPLValue)]
unify exps vals = let exps' = map squash exps
                      vals' = map squash vals in
                  zip exps' vals' >>= unifyExp
  where unifyExp ((Id name), val) = [(name, val)]
        unifyExp ((List ls), (List val)) = unify ls val
        unifyExp (ls@(List _), val)      = unifyExp (ls, (List [val]))
          
squash :: TPLValue -> TPLValue
squash (Expression [val]) = squash val
squash (Sequence [val])   = squash val
squash val                = val