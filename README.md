TPL
===

## Summary
A silly little programming language that I'm working on for fun. I'm writing it in [Haskell](http://www.haskell.org) using the awesome [Parsec library](http://www.haskell.org/haskellwiki/Parsec) for parsing.

Currently the idea is for it to be simple, dynamically typed, interpreted, impure and eagerly evaluated--basically the exact opposite of Haskell, which is the language I'm using to implement it.

## Progress
So far I have some basic operators like +, -, *, / working with rampant type coercion. Equality (== and !=) also work with type coercion. Additionally >< concatenates strings since using + in a language with weak typing for concatenation is just silly (*cough* JavaScript). 

Lambdas are written in the form `\arg1, arg2 -> body`. They are parsed correctly but are completely useless.

More stuff should be along shortly. Hopefully.
