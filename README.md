# TPL

TPL is a dynamically typed, imperative, interpreted scripting language. The main design goal was flexibility coupled with simplicity: the base language is very minimal, with features powerful enough to meaningfully extend it. For example, you can write your own control structures like loops.

TPL is heavily influenced by JavaScript, Haskell and Scheme; I also took ideas from other languages I've used. 

## Documentation

Read all about the language in the [Guide](http://inst.eecs.berkeley.edu/~tikhon/tpl)

This is also availabile as a Markdown page on [Github](https://github.com/TikhonJelvis/TPL/blob/master/Guide.md).

## Installation Instructions

To install, clone this repository and use `cabal`:

    git clone git://github.com/TikhonJelvis/TPL.git
    cd TPL
    cabal install

If you use Emacs, you can also install a simple Emacs mode for TPL. To do this, copy `emacs/tpl-mode.el` to somewhere in your load path and add `(require 'tpl-mode)` to your `.emacs` file. This will automatically associate `.tpl` files with `tpl-mode`.

## Future Features:

In the near future, I want to add:
  
  - better error handling and reporting
  - more standard functions (IO is particularly lacking right now)
  - an OOP system similar to JavaScript or Lua and unified with the scoping system

## Example Code

Check out the `base` library for some example code. The library is in several files inside the `src/base` directory.

The library is mostly written in a very functional style. However, there are some interesting functions like `partitionBy` in `base/list.tpl` that are written in a more imperative style.
