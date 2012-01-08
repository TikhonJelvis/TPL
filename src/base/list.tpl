fn >> ls := map fn ls;
ls << fn := fn >> ls;
for := (<<);

length [x, xs...] := if (x = null) 0 else 1 + length xs;
head [a] := a;
tail [_, xs...] := xs;

map fn [x, xs...] := if (x = null) [] else fn x : map fn xs;

filter pred [x, xs...] := if (x = null) []
                     else if (pred x)   x : filter pred xs
                     else               filter pred xs;

fold fn base [x, xs...] := if (x = null) base else fold fn (fn base x) xs;
fold1 fn [x, xs...] := fold fn x xs;

zip [x, xs...] [y, ys...] := if (x = null & y = null) [] else [x, y] : zip xs ys;
unzip [[xs, ys]...] := [xs, ys];

take n [x, xs...] := if (ls & n) x : take (n - 1) xs else [];
drop n [x, xs...] := if (ls) {if (n) drop (n - 1) xs else x:xs} else [];
sub start end ls := take (end - start) $ drop start ls;

repeat x n := if (n) x : repeat x (n - 1) else [];
