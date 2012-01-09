load 'base/function';

map fn [x, xs...] := if (is x) fn x : map fn xs else [];

fold fn base [x, xs...] := if (is x) fold fn (fn base x) xs else base;
fold1 fn [x, xs...] := fold fn x xs;

is x  := x /= null;
isnt  := not . is;
are   := fold (&) . map is;

a .. b := if (a > b) reverse $ b..a 
   else if (a = b) [a] 
   else            a : (succ a .. b);

[x, xs...] ! i := if (i) xs ! pred i else x;

fn >> ls := map fn ls;
ls << fn := fn >> ls;
for := (<<);

[x, xs...] ++ ls := if (is x) x : (xs ++ ls) else ls;

length [x, xs...] := if (is x) 1 + length xs else 0;
head [a] := a;
tail [_, xs...] := xs;

filter pred [x, xs...] := if (isnt x) []
                     else if (pred x)   x : filter pred xs
                     else               filter pred xs;

zipWith fn [x, xs...] [y, ys...] := if (are [x, y]) []
                                  else fn x y : zipWith fn xs ys;
zip := zipWith (:);
unzip [[xs, ys]...] := [xs, ys];

partitionBy pred ls := {
  l := [];
  r := [];
  for ls \ x -> 
      if (pred x) l <- x : l 
    else          r <- x : r;
  [l, r];
};

take n [x, xs...] := if (is x & n) x : take (n - 1) xs else [];
drop n [x, xs...] := if (isnt x) [] else if (n) drop (n - 1) xs else x:xs;
sub start end ls := take (end - start) $ drop start ls;

reverse [x, xs...] := if (isnt x) [] else reverse xs ++ x;

repeat x n := if (n) x : repeat x (n - 1) else [];
