a && b := if (a) b else false;
a || b := if (a) a else b;
not a  := if (a) false else true;
a ^ b  := if (a) not b else b;

fn >> ls := map fn ls;
ls << fn := fn >> ls;

empty ls := length ls = 0;
map fn ls := if (empty ls) [] else fn (head ls) : map fn (tail ls);
filter pred ls := 
  if (empty ls) 
    [] 
  else if (pred (head ls)) 
    head ls : filter pred (tail ls) 
  else
    filter pred (tail ls);
fold fn accum ls := if (empty ls) accum 
                    else fold fn (fn accum (head ls)) (tail ls);
