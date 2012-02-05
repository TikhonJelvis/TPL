require 'base/control'
require 'base/function'
require 'base/logic'
require 'base/math'

map fn [x, xs...] := is x --> (fn x : map fn xs) | []

fold fn base [x, xs...] := x & (fold fn (fn base x) xs) | base
fold1 fn [x, xs...] := fold fn x xs

is x  := x /= null
isnt  := not . is
are   := fold (&) . map is

a .. b := cond [
    a > b -> reverse (b..a),
    a = b -> [a],
    true  -> a : (succ a .. b)
]

[x, xs...] ! i := if (i) {
    xs ! pred i
} else {
    x
}

fn >> ls := map fn ls
ls << fn := fn >> ls

[x, xs...] ++ ls := x & (x : (xs ++ ls)) | ls

head [a] := a
tail [_, xs...] := xs

init [x1, x2, xs...] := if (is xs) {
    [x1, x2] ++ init xs 
} else @ if (is x2) x1 else []
last [x, xs...] := if (is xs) {last xs} else x

element ~> [x, xs...] := x --> (element = x) | (element ~> xs)

filter pred [x, xs...] := x & (pred x & x : filter pred xs | filter pred xs) | []

zipWith fn [x, xs...] [y, ys...] := x & y & fn x y : zipWith fn xs ys | []
zip := zipWith (:)
unzip [[xs, ys]...] := [xs, ys]

partitionBy pred ls := {
  l := []
  r := []
  for x in ls {
      pred x & l <- x : l | r <- x : r
  }
  [l, r]
}

groupBy fn ls := {
  curr := null
  res  := []
  for x in ls {
    if (fn x = curr) {
        res <- init res ++ [last res ++ [x]] 
    } else {
        res <- res ++ [[x]]
    }
    curr <- fn x
  }
  res
}

take n [x, xs...] := if (is x & n) {x : take (n - 1) xs} else []
drop n [x, xs...] := x & (n & drop (n - 1) xs | x:xs) | []
sub start end ls  := take (end - start) @ drop start ls

reverse [x, xs...] := x & reverse xs ++ x | []

repeat x n := n & (x : repeat x (n - 1)) | []

and := fold1 (&)
or  := fold1 (|)

none ls := filter id ls = []
