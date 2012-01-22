not b := if (b) false else true;

a <= b := not (a > b);
a >= b := a = b | a > b;
a <  b := not (a >= b);

succ n := n + 1;
pred n := n - 1;

load 'base/function'
     'base/list'
     'base/logic'
     'base/math';

$case -> $result := \ x -> if (x = case) result;
switch value [case, rest...] := case value | (rest & switch value rest)
