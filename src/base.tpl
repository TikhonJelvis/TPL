define "*precs*" {
    "+" : 5
    "-" : 5
    "*" : 6
    "/" : 6
    "%" : 6
    ":=" : 1
    "<-" : 2
    "=" : 3
    "/=" : 3
    ":" : 4
    "*application*" : 10
    "." : 11
}

precedence $op n := defineObj (get "*precs*") (exprToString op) n
precedenceOf $op := getObj (get "*precs*") (exprToString op)

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

let bindings $body := (
    bindings :> get "*context*"
    force (with bindings body)
)

-- require 'base/control'
-- require 'base/list'
-- require 'base/function'
-- require 'base/logic'
-- require 'base/math'
-- require 'base/string'
