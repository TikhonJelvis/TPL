require 'base/function'
require 'base/list'

condition => $result := if condition result else false
precedence (=>) 2
else := true

condition ? $consequent := if condition (\ $x -> consequent) else (\ $x -> x)
precedence (?) 4

$a -> $b := [$a, $b]
precedence (->) 2
switch value [case, rest...] := (
  compare [condition, res] test := condition = test => res
  compare case value | (is rest => switch value rest)
)
cond [[condition, result], rest...] := (is condition | is result) => (condition ? result # cond rest)

for $x $in ls $body := (
    bindingOf item := (
        obj := {}
        defineObj obj (exprToString x) item
        obj
    )
    map (λ item -> force (with (bindingOf item) body)) ls
)

while $condition $body := condition ? body : while condition body @ []
do $body $while $condition := body : (condition & while condition body) | []
