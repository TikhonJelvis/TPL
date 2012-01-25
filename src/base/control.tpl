load 'base/list';

$case -> $result := \ x -> if (x = case) result;
switch value [case, rest...] := case value | (rest & switch value rest);

for $x $in ls $body := map (\ item -> with [x -> item] body) ls;

while $cond $body := cond & (body : while cond body) | [];
do $body $while $cond := body : (cond & while cond body) | [];

