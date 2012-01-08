load 'base/list';

a % b := a - (a / b * b);

a ^ b := fold1 (*) $ repeat a b;

a <= b := not (a > b);
a >= b := a = b | a > b;
a <  b := not (a >= b);

sum ls := fold1 (+) ls;
avg ls := sum ls / length ls;

succ n := n + 1;
pred n := n - 1;

v1 |*| v2 := sum . map (\ [a, b] -> a * b) $ zip v1 v2;
