# TPL

TPL is a dynamically typed, imperative, interpreted scripting language. The main design goal was flexibility coupled with simplicity: the base language is very minimal, with features powerful enough to meaningfully extend it. For example, you can write your own control structures like loops.

TPL is heavily influenced by JavaScript, Haskell and Scheme; I also took ideas from other languages I've used. 

## Documentation

The only documentation is currently a relatively thorough [user guide](http://inst.eecs.berkeley.edu/~tikhon/tpl).

It is available as a markdown file called `Guide.md` in this repository as well.

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
