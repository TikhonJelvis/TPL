require 'base/list'

cond --> $result := if cond result else false
precedence (-->) 7
else := true

$a -> $b := [$a, $b]
precedence (->) 11
switch value [case, rest...] := {
  compare [cond, res] test := cond = test & res
  compare case value | (rest & switch value rest)
}
cond [[condition, result], rest...] := if condition result else (is rest --> cond rest)

for $x $in ls $body := map (\ item -> with [x -> item] body) ls

while $cond $body := cond & (body : while cond body) | []
do $body $while $cond := body : (cond & while cond body) | []
