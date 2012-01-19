load 'base/function';
load 'base/list';
load 'base/logic';

a % b  := a - (a / b * b);
even n := (n % 2) = 0;
odd    := not . even;

a ^ b := fold1 (*) @ repeat a b;

sum ls := fold1 (+) ls;
avg ls := sum ls / length ls;

v1 |*| v2 := sum . map (\ [a, b] -> a * b) @ zip v1 v2;
