require 'base/function'
require 'base/list'

condition --> $result := if condition result else false
precedence (-->) 9
else := true

condition ? $consequent := if condition (\ $x -> consequent) else (\ $x -> x)
precedence (?) 10

$a -> $b := [$a, $b]
precedence (->) 11
switch value [case, rest...] := (
  compare [condition, res] test := condition = test --> res
  compare case value | (is rest --> switch value rest)
)
cond [[condition, result], rest...] := (is condition | is result) --> (condition ? result @ cond rest)

for $x $in ls $body := map (λ item -> with [x -> item] body) ls

while $condition $body := condition ? body : while condition body @ []
do $body $while $condition := body : (condition & while condition body) | []
