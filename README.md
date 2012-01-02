TPL
===

## Summary 

A silly little programming language that I'm working on for
fun. I'm writing it in [Haskell](http://www.haskell.org) using the
awesome [Parsec library](http://www.haskell.org/haskellwiki/Parsec)
for parsing.

Currently the idea is for it to be simple, dynamically typed,
interpreted, impure and eagerly evaluated--basically the exact
opposite of Haskell.

This language isn't really meant for use; it's just a fun didactic
exercise. Besides, everybody want to have their own language...

## Some code:

'''
empty := \ ls -> length ls = 0
map := \ fn ls -> if (empty ls) [] else fn (head ls) : map fn (tail ls)
succ := \ n -> n + 1
map succ (1..10)

fact := \ n -> if (n = 0) 1 else n * (fact n - 1)
fib := \ n -> if (n = 0 | n = 1) n else (fib n - 1) + (fib n - 2)

n := 0
addToN := \ num -> n <- n + num

sum := \ ls -> if (empty ls) 0 else head ls + sum (tail ls)

fold := \ fn accum ls -> if (empty ls) accum else fold fn (fn accum (head ls)) (tail ls)
add := \ a b -> a + b
sum2 := \ ls -> fold add 0 ls
'''

Basically, `:=` defines variables while `<-` mutates them--the
behavior is almost identical to `define` and `set!` in Scheme. 

All expressions have values; practically anything can be an
expression. Particularly, a block of code (between { and }) is an
expression with the value of its last expression; it can be put
anywhere a normal expression could be. So:

'''
blarg := \ n -> if ({a := n; n <- n + 1; a + n} = 101) "blarg" else "not blarg"
'''
is valid and `blarg 50` will return the string `blarg`.

In the near future, I want to add pattern matching, custom operators
and maybe prototype-based objects like JavaScript. I'm also fixing
some of the stupider issues, particularly with operator precedence,
errors and parsing.
