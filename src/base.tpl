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
    "#" : 11
}

precedence $op n := defineObj (get "*precs*") (exprToString op) n
precedenceOf $op := getObj (get "*precs*") (exprToString op)

print x := puts (toString x)

_modules := []
require f := (
    inLs e [x, xs...] := if (x = null) false (if (x = e) true (inLs e xs))
    if (inLs f _modules) null (
        fullName := TPL_PATH >< '/' >< f >< '.tpl'
        loadObj (get "*current*") fullName
        _modules <- f : _modules
    )
)

let $bindings $body := with bindings body

require 'base/control'
require 'base/list'
require 'base/function'
require 'base/logic'
require 'base/math'
require 'base/string'