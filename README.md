# TPL

TPL is a dynamically typed, imperative, interpreted scripting language. The main design goal was flexibility coupled with simplicity: the base language is very minimal, with features powerful enough to meaningfully extend it. For example, you can write your own control structures like loops.

TPL is heavily influenced by JavaScript, Haskell and Scheme; I also took ideas from other languages I've used. 

## Installation Instructions

To install, clone this repository and use `cabal`:

    git clone git://github.com/TikhonJelvis/TPL.git
    cd TPL
    cabal install

## Documentation

The only documentation is currently a relatively thorough [user guide](http://inst.eecs.berkeley.edu/~tikhon/tpl).

It is available as a markdown file called [`Guide.md`](https://github.com/TikhonJelvis/TPL/blob/master/Guide.md) in this repository as well.

## Flexibility

One of the core tenants of TPL is flexibility: you should be able to meaningfully extend the language *in* the language. 

You could want to add your own control structures. For example, the language does not have any sort of `switch` statement by default. Happily, you can write one yourself:

    $case -> $result := \ x -> if (x = case) result;
    switch value [case, rest...] := case value | (rest & switch value rest)

This could then be used like this:

    switch a [
      1 -> "one",
      2 -> "two",
      3 -> "three"
    ];

You can also use dynamic scoping using the `with` native function, if you want. For example:

    for $x $in ls $body := map (\ item -> with [x -> item] body) ls;
    for i in (1..10) {
      a := i + 1;
      b := i - 1;
      print @ b >< " " >< i >< " " >< c;
    };

Here `x` holds a *variable name* and `with` is used to set whatever `x` is to whatever `item` is inside of `body`. This means that whatever id `x` holds is dynamically typed. Magic!

## Future Features:

In the near future, I want to add:
  
  - more extensive and useful pattern matching (not just destructuring assignment for lists).
  - better error handling and reporting
  - more standard functions (IO is particularly lacking right now)
  - laziness on demand--I want functions to be able to control which of their parameters gets evaluated
  - maybe an OOP system of some kind--most likely prototype-based (no classes)

## Example Code

Check out the `base` library for some example code. This comes in five files: `base.tpl`, `base/function.tpl`, `base/list.tpl`, `base/logic.tpl` and `base/math.tpl`.

This is just the standard library for the language. Right now it is *not* loaded by default; any serious code should have a `load 'base';` statement at the top. 

The library is mostly written in a very functional style. However, there are some interesting functions like `partitionBy` in `base/list.tpl` that are written in a more imperative style.
