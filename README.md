TPL
===

## Summary A silly little programming language that I'm working on for
fun. I'm writing it in [Haskell](http://www.haskell.org) using the
awesome [Parsec library](http://www.haskell.org/haskellwiki/Parsec)
for parsing.

Currently the idea is for it to be simple, dynamically typed,
interpreted, impure and eagerly evaluated--basically the exact
opposite of Haskell, which is the language I'm using to implement it.

## Progress So far I have some basic operators like `+, -, *, /`
working with rampant type coercion. Equality (`==` and `!=`) also work
with type coercion. Additionally `><` concatenates strings since using
`+` in a language with weak typing for concatenation is just silly
(*cough* JavaScript).

Lambdas are written in the form `\arg1 arg2 -> body`. They can be
assigned to a variable (`a = \ a b -> a + b`) and then used.

Two operators for working with lists have been added: `:` is cons and
`!!` is index. Both of these have been blatantly adopted from
Haskell. Both can coerce non-list values into lists of just that
value. That is, `1 : 2` is the same as `1 : [2]`.

Booleans are supported, as is a c-style if statement, although it has
not yet been thorougly tested.

Currently dealing with more than one expression anywhere does not work
very well either; this really needs to be fixed.

More stuff should be along shortly. Hopefully.
