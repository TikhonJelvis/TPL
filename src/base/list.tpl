require 'base/control'
require 'base/function'
require 'base/logic'
require 'base/math'

map fn [x, xs...] := is x ? fn x : map fn xs # []

filter pred [x, xs...] := cond [
    is x & pred x => x : filter pred xs,
    is x          => filter pred xs,
    else          => []
]

fold fn acc [x, xs...] := is x ? fold fn (fn acc x) xs # acc
fold1 fn [x, xs...] := fold fn x xs

a .. b := cond [
    a > b => reverse (b..a),
    a = b => [a],
    else  => a : (succ a .. b)
]
precedence (..) 5

repeat x n := n > 0 ? x : repeat x (n - 1) # []

[x, xs...] ! i := i > 0 ? xs ! pred i # x
precedence (!) 4

head [x, xs...] := x
tail [x, xs...] := xs

precedence (++) 7
[x, xs...] ++ [y, ys...] := is x ? x : xs ++ (y:ys) # (is y ? y:ys # ys)

init [x1, x2, xs...] := cond [
    xs /= [] => [x1, x2] ++ init xs,
    is x2    => [x1],
    else     => []
]
last [x, xs...] := is xs ? last xs # x

