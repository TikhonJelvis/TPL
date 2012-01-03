TPL
===

## Summary 

A silly little programming language that I'm working on for fun. I'm writing it in [Haskell](http://www.haskell.org) using the awesome [Parsec library](http://www.haskell.org/haskellwiki/Parsec) for parsing.

Currently the idea is for it to be simple, dynamically typed, interpreted, impure and eagerly evaluated--basically the exact opposite of Haskell.

This language isn't really meant for use; it's just a fun didactic exercise. Besides, everybody want to have their own language...

## Future Features:

In the near future, I want to add:
  
  - pattern matching (at least for lists)
  - better error handling and reporting
  - functions with no arguments (they can't really be called right now...)
  - more standard functions (both native and part of `base.tpl`)


## Details:

`:=` and `<-` are broadly equal to `define` and `set!` from Scheme.

Functions are called without parentheses; `f a b c` is equal to `f(a, b, c)` in C-like languages. You can define functions in two different ways:

'''
f := \ n -> n + 1;
f n := n + 1;
'''

`\ a b c -> ...` is a lambda expression and can be used anywhere a normal expression can.

All expressions have values; practically anything can be an expression. Particularly, a block of code (between { and }) is an expression with the value of its last expression; it can be put anywhere a normal expression could be. So:

'''
blarg := \ n -> if ({a := n; n <- n + 1; a + n} = 101) "blarg" else "not blarg";
'''

is valid and `blarg 50` will return the string `blarg`.

You can define custom operators. For example, `<<` is equal to `map` in the standard library:

'''
ls << fn := map fn ls;
'''

You can also load other files. This will run those files in your current environment (so if you load a file inside a function, things from that file will only be available in *that* function!).

'''
load 'base';
'''

You do *not* include the .tpl extension when loading files. `base.tpl` is the standard library and contains things like `map` that don't need to be native.
