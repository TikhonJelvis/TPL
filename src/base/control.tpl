require 'base/list'

condition --> $result := if condition result else false
precedence (-->) 7
else := true

$a -> $b := [$a, $b]
precedence (->) 11
switch value [case, rest...] := {
  compare [condition, res] test := condition = test --> res
  compare case value | is rest --> switch value rest
}
cond [[condition, result], rest...] := condition --> result | is rest --> cond rest

for $x $in ls $body := map (\ item -> with [x -> item] body) ls

while $condition $body := condition --> (body : while condition body) | []
do $body $while $condition := body : (condition & while condition body) | []
