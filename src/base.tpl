-- ⟦abc⟧
define "*precs*" {
    ":="            : 1
    "<-"            : 2
    "="             : 5
    "/="            : 5
    ":"             : 6
    "+"             : 7
    "-"             : 7
    "*"             : 8
    "/"             : 8
    "%"             : 8
    "*application*" : 12
    "."             : 13
    
    "*get*"         : λ _ -> 6 -- the default precedence
}

-- TODO: Make sure this works for delayed values! (Perhaps typeof $x = "delayed"?)
flip fn a b := (
    res := fn b
    a || $b := if a true else b
    if ((typeof res = "function") || (typeof res = "native")) (res a) else res
)

trimPrecs str := substr str 1 (strlen str - 1)
precedence $op n := defineObj (get "*precs*") (trimPrecs (exprToString op)) n
precedenceOf $op := getObj (get "*precs*") (trimPrecs (exprToString op))

print x := puts (toString x)

obj :> parent := (
    defineObj obj "*parent*" parent
    obj
)
precedence (:>) 3

if cond $res $else $alt := _if cond res alt

_modules := []
require f := (
    inLs e [x, xs...] := if (x = null) false else (if (x = e) true else (inLs e xs))
    if (inLs f _modules) null else (
        _modules <- f : _modules
        fullName := TPL_PATH >< '/' >< f >< '.tpl'
        loadObj (get "*context*") fullName
    )
)

force x := x

require 'base/logic'
require 'base/function'
-- require 'base/control'
-- require 'base/list'
-- require 'base/math'
-- require 'base/string'
