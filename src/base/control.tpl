load 'base/list';

$a -> $b := [$a, $b];

switch value [case, rest...] := {
  compare [cond, res] test := cond = test & res;
  compare case value | (rest & switch value rest);
};

for $x $in ls $body := map (\ item -> with [x -> item] body) ls;

while $cond $body := cond & (body : while cond body) | [];
do $body $while $cond := body : (cond & while cond body) | [];

cond [[condition, result], rest...] := if (condition) result 
                                     else rest & cond rest;
