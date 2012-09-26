require 'base/control'
require 'base/function'
require 'base/logic'
require 'base/math'

map fn [x, xs...] := is x ? fn x : map fn xs # []

fold fn acc [x, xs...] := is x ? fold fn (fn acc x) xs # acc
fold1 fn [x, xs...] := fold fn x xs

a .. b := cond [
    a > b => reverse (b..a),
    a = b => [a],
    else  => a : (succ a .. b)
]
precedence (..) 5

repeat x n := n > 0 ? x : repeat x (n - 1) # []