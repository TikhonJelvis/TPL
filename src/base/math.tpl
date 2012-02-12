require 'base/function'
require 'base/list'
require 'base/logic'

a // b := a - (a / b * b)
even n := (n % 2) = 0
odd    := not . even

precedence (>) 7

a <= b := not (a > b)
precedence (<=) 7

a >= b := a = b | a > b
precedence (>=) 7

a <  b := not (a >= b)
precedence (<)  7

succ n := n + 1
pred n := n - 1

a ^ b := fold1 (*) @ repeat a b

sum ls := fold1 (+) ls
avg ls := sum ls / length ls

v1 |*| v2 := sum . map (\ [a, b] -> a * b) @ zip v1 v2
