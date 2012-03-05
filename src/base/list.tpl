require 'base/control'
require 'base/function'
require 'base/logic'
require 'base/math'

map fn [x, xs...] := is x ? fn x : map fn xs @ []

fold fn base [x, xs...] := is x ? fold fn (fn base x) xs @ base
fold1 fn [x, xs...] := fold fn x xs

is x := x /= null
isnt := not . is
are  := fold (&) . map is

a .. b := cond [
    a > b -> reverse (b..a),
    a = b -> [a],
    true  -> a : (succ a .. b)
]

[x, xs...] ! i := i ? xs ! pred i @ x

fn >> ls := map fn ls
ls << fn := fn >> ls

[x, xs...] ++ ls := is x ? (x : (xs ++ ls)) @ ls

head [a] := a
tail [_, xs...] := xs

init [x1, x2, xs...] := cond [
    is xs -> [x1, x2] ++ init xs,
    is x2 -> x1,
    else  -> []
]
last [x, xs...] := is xs ? last xs @ x

element ~> [x, xs...] := is x --> element = x | element ~> xs
precedence (~>) 7

filter pred [x, xs...] := cond [
    is x & pred x -> x : filter pred xs,
    is x          -> filter pred xs,
    else          -> []
]

zipWith fn [x, xs...] [y, ys...] := (is x & is y) ? (fn x y : zipWith fn xs ys) @ []
zip := zipWith (:)
unzip [[xs, ys]...] := [xs, ys]

partitionBy pred ls := (
    l := []
    r := []
    for x in ls (
        if (pred x) (
            l <- l ++ [x]
        ) else (
            r <- r ++ [x]
        )
    )
    [l, r]
)

groupBy fn ls := (
  curr := null
  res  := []
  for x in ls (
    if (fn x = curr) (
        res <- init res ++ [last res ++ [x]] 
    ) else (
        res <- res ++ [[x]]
    )
    curr <- fn x
  )
  res
)

take n [x, xs...] := if (is x & n) (
    x : take (n - 1) xs
) else (
    []
)
drop n [x, xs...] := is x & n ? drop (n - 1) xs @ x:xs
sub start end ls  := take (end - start) @ drop start ls

reverse [x, xs...] := is x ? (reverse xs ++ x) @ []

repeat x n := n & (x : repeat x (n - 1)) | []

and := fold1 (&)
or  := fold1 (|)

none ls := filter id ls = []
