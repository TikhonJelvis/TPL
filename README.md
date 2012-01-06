TPL
===

## Summary 

A silly little programming language that I'm working on for fun. I'm writing it in [Haskell](http://www.haskell.org) using the awesome [Parsec library](http://www.haskell.org/haskellwiki/Parsec) for parsing.

Currently the idea is for it to be simple, dynamically typed, interpreted, impure and eagerly evaluated--basically the exact opposite of Haskell.

This language isn't really meant for use; it's just a fun didactic exercise. Besides, everybody wants to have their own language...

## Future Features:

In the near future, I want to add:
  
  - more extensive and useful pattern matching (not just destructuring assignment for lists).
  - better error handling and reporting
  - functions with no arguments (they can't really be called right now...)
  - more standard functions (both native and part of `base.tpl`)


## Details:

### Basics

`:=` and `<-` are broadly equal to `define` and `set!` from Scheme.

Functions are called without parentheses; `f a b c` is equal to `f(a, b, c)` in C-like languages. You can define functions in two different ways:

    f := \ n -> n + 1;
    f n := n + 1;

`\ a b c -> ...` is a lambda expression and can be used anywhere a normal expression can.

All expressions have values; practically anything can be an expression. Particularly, a block of code (between { and }) is an expression with the value of its last expression; it can be put anywhere a normal expression could be. So:

    blarg := \ n -> if ({a := n; n <- n + 1; a + n} = 101) "blarg" else "not blarg";

is valid and `blarg 50` will return the string `blarg`.

### Operators

You can define custom operators. For example, `<<` is equal to `map` in the standard library:

    ls << fn := map fn ls;

You can also use "operator slices" like in Haskell:
   
    load 'base';
    map (+ 2) (1..10)

This code will add 2 to each of the list's items. The `+ 2` expression actually evaluates to a function in the form `λ α → α + 2`; the greek letter α is used because it cannot currently be used as a normal identifier. The opposite expression would have worked too: `2 +` results in `λ α -> 2 + α`. I suspect bad things might happen if you try to nest slices, so don't.

### Pattern Matching (Destructuring Assignment)

There is *very basic* pattern matching, which is really like "destructuring assignment" from JavaScript. I want to have proper pattern matching, but that comes later (if at all :)).

    [a, b] := [37, 42]
    [a, b] <- (1..10)
    [c, d] := 1

Note how the two size-mismatched cases are both valid. In `[a, b] <- (1..10)`, `a` is `1` and b is `2`; the rest is thrown away. In `[c, d] := 1`, `c` is 1 and `d` is not defined.

You can also use these patterns in functions:

    f [a, b] := a + b
    f [1, 2]

You cannot have multiple declarations of a function with different patterns yet. This is part of the "proper" pattern matching I want to add in the future.

Finally, you can also nest these patterns:

    [a, [b, c], d] := [1, [2, 3], 4]

### Files and IO

You can also load other files. This will run those files in your current environment (so if you load a file inside a function, things from that file will only be available in *that* function!).

    load 'base';

You do *not* include the .tpl extension when loading files. `base.tpl` is the standard library and contains things like `map` that don't need to be native.

Since `load` is just a (native) function, you can pass it around like normal:

    map load ['base', 'example', 'blarg', 'stuff '];

You can also get the contents of a file as a string with `open`. This *does* need the extension:

    str := open 'blarg.txt';

You can also print to STDOUT with `print`:

    print "blarg";
    print 5;
    print (+);

This should work on *all* values including functions.
