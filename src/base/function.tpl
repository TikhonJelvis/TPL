id x := x;
ignore x := null;
const x y := x;

fn @ arg := fn arg;
f . g := \ x -> f (g x);

binary <.> unary := \ a b -> binary (unary a) (unary b);

flip fn a b := fn b a;
