id x := x;
ignore x := null;

fn $ arg := fn arg;

not a := if (a) false else true;
a ^ b := if (a) not b else b;

a % b := a - (a / b * b);

fn >> ls := map fn ls;
ls << fn := fn >> ls;
for := (<<);

head [a] := a;

map fn ls := if (not ls) [] else fn (head ls) : map fn (tail ls);

filter pred ls := 
  if (not ls) 
    [] 
  else if (pred $ head ls) 
    head ls : filter pred (tail ls) 
  else
    filter pred (tail ls);

fold fn accum ls := if (not ls) accum 
                    else fold fn (fn accum $ head ls) $ tail ls;
fold1 fn ls := fold fn $ head ls $ tail ls;

zip l1 l2 := if (not l1 & not l2) []
        else if (not l1) map (\ n -> [null, n]) l2
        else if (not l2) map (\ n -> [n, null]) l1
        else    [head l1, head l2] : zip $ tail l1 $ tail l2;

take n ls := if (ls & n) head ls : take (n - 1) (tail ls) else [];
drop n ls := if (ls) {if (n) drop (n - 1) $ tail ls else ls} else [];
sub start end ls := take (end - start) $ drop start ls;

repeat x n := if (n) x : repeat x (n - 1) else [];
