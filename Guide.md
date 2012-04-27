# User Guide

TPL is a simple dynamically typed scripting language. The idea behind the language is to combine minimalism with flexibility--the language has a small core flexible enough to move things like control structures into libraries. This means, naturally, that you are free to design your own control structures or even replace the standard ones.

I'm still playing around with the design of the language and adding features; even core parts may change drastically in the near future.

## Installation

The language interpreter requires Haskell and Cabal. If you do not have Haskell installed on your system, I suggest the [Haskell Platform](http://hackage.haskell.org/platform/). This is packaged for every major operating system, comes with Cabal and the correct version of Haskell.

When you have Haskell installed, you need to download the TPL code. If you have `git` installed, run:

    git clone git://github.com/TikhonJelvis/TPL.git

If you do not want to use `git`, you can also download the code as a [zip file](https://github.com/TikhonJelvis/TPL/zipball/master).

Once you have the code, enter the its directory:

    cd TPL

and run:

    cabal install

This will install the TPL interpreter and its dependencies.

## Language

### Literals

Numbers are just themselves. So `1` is a `1`. Floating point numbers are not currently supported, but they're overrated anyhow ;).

Strings can be surround by either `"` or `'`. So `"abc"` and `'abc'` are the same.

Null and boolean literals are all lower-case: `null`, `true`, `false`.

Lists look like array literals from JavaScript and can be nested: `[1,2,"abc","def",[5,6,7]]`.

### Variables

Each program is a series of statements, much like other scripting languages (Python, Ruby, JavaScript...). You can put each statement on a newline or use semi-colons to separate multiple statements on the same line.

You can declare a variable with `:=`:

    x := 10

You can change the value of a variable with `<-`. 

    x <- 11

If `x` has not been declared yet, this will result in an error.

### Functions

You can write a function as follows:

    \ a b -> a + b

This lets you define a named function:

    fn := \ a b -> a + b

As this is a very common operation, there is some special syntax for it:

    fn a b := a + b

Each function creates a new scope and forms a closure. In essence, this means scoping behaves exactly like in Scheme or JavaScript. Unlike Python, you can reassign the value of a variable declared outside of a function:

    x := 10
    fn a := x <- a

Here `fn` will set `x` to the value of `a`. Had `:=` been used in place of `<-`, it would have created a new variable called `x` in the scope of `fn` without affecting the `x` in the outer scope.

Functions are applied using Haskell-style syntax. This means that `f a b c` is like `f(a, b, c)` in JavaScript/Python or `(f a b c)` in Scheme. Parentheses are only used for grouping, so `f (a b)` is the same as `f(a(b))` in JavaScript rather than `f(a, b)`.

Functions can be partially applied. That is, given:

    f a b := a + b

the expression `f 1` is equal to `\ α -> f 1 α`.

You can define a function that has a body containing multiple statements. This is done by surrounding the body in parentheses. Just like in the body of the program, you can separate statements by semi-colons or newlines.

    f x := (
      a := x + 10
      a + 11
    )

The value of the last statement executed will be returned. 

You can actually put "blocks" of code like this anywhere you could put a normal expression.

How you indent code like this is, naturally, completely up to you.

## Laziness

If you declare a function taking *no* arguments, it isn't immediately clear how to use it. Other functions get called when you pass parameters into them (`f 1 2 3`), but how do you call one without any parameters?

In fact, you call a function like that just by referencing it. This means that a function of no arguments acts like a deferred expression rather than normal function. Also, from the perspective of the caller, you cannot tell a function of no arguments from a normal value.

For convenience, you can declare a function like this (or, equivalently, defer a statement) using a `$`. So, given:

    x := $(y + z)

`x` will be the sum of the *current* value of `y` and `z` each time you reference it. This makes basic reactive programming very easy, and lets you control when a statement gets executed.

Functions can also control which of their parameters to execute. If you use `$` on a function's parameter, it will not get evaluated when the function is called. So:

    f a $b := a

will *never* evaluate `b`. If you call `f a (print "blarg")` nothing will be printed.

This also works on patterns (like `f $[a, b]`).

## Custom Scope

You can add bindings to the environment of a closure using the `with` native function. Given a closer `f`, we can write:

    g := with ["x" -> 11] f

Now `g` is the same function as `f` except that `x` will be equal to `11` inside it.

If `with` is given a deferred statement, that statement is executed immediately. So:

    with ["x" -> 11] $(x^2)

will return 121.

If you pass `with` a deferred variable name to bind to, it unpacks this. So:

    name := $x
    with [name -> 11] $(x^11)

is the same as the previous example (121). This is useful for control structures like the `for .. in` loop.

This `with` function may seem a little arbitrary. And it is. In fact, this is just a placeholder until I get objects working; see the end of this guide.

### Operators

Operators are just normal functions which are in infix position by default. You can define your own operators:

    x ~ y := if (x > y) x else y

You can also partially apply operators. `(1 ~)` is equal to `\ α -> 1 ~ α`.

You can use operators in prefix position by surrounding them in parentheses. So `1 + 2` and `(+) 1 2` are identical. This is just like partially applying the operator to no arguments.

You can also use normal functions in infix position by surrounding them with backticks. So `f a b` could be written as ``a `f` b``. You can partially apply these the same way as operators.

When you define an operator, you can set its precedence as follows:

    precedence (~) 2

The precedence should be a number between 1 and 11. You can get an operators precedence with the `precedenceOf` function:

    precedenceOf (~)

### Destructuring Assignment

You can destructure lists just like in newer version of JavaScript. For example, `[x, y] := [1, 2]` sets `x` to `1` and `y` to `2`. This works with both `:=` and `<-`.

These patterns can be nested as well:

    [a, [b, c], d] := [1, [2, 3], 4]

You can match the rest of the list with `...`:

    [x, xs...] := [1, 2, 3, 4, 5]

This also works on nested patterns.

These patterns can also be used in function declarations.

    head [x, xs...] := x

This defines `head` to return the first element of a list. If the list is empty, `x` would be `null`.

## Standard Library

### Basic Operators

The language comes with the arithmetic operators you would expect from a language like JavaScript. The main differences are `=` for equality, `/=` for inequality, `//` for mod and `^` for power. Bitwise operations are not supported.

The `%` operator is used for string formatting, like in Python. However, only `%s` and `%d` patterns are supported right now, so you can only really show things as strings or numbers. The following are equivalent:

    "A number: %d and a string: %s." % [1, "blarg"]
    "A number: 1 and a string: blarg."

For lists, `:` is cons and `++` is append. So `1:[2,3,4]` gives `[1,2,3,4]` as does `[1,2] ++ [3, 4]`. You can get a range with `..`. So `[1,2,3,4,5]` is `1..5`.

Logic operators are simple: `&` is and and `|` is or. 

`@` is the function application operator. This lets you write fewer parentheses: `f (a b c)` can be written as `f @ a b c`. 

We can combine this with `?` to get a ternary conditional operator:

     x := a > b ? 10 * a @ 12 * b

There is also an "implication" operator `-->`. `a --> b` is equivalent to `not a | b`.

Finally, there is a very useful composition operator `.`. `f . g` is equivalent to:

    \ x -> f (g x)

This is just like function composition in math (f ∘ g).

### Control Structures

Control structures are also part of the standard library. There are several defined.

When using control structures like this, it is often useful to pass in blocks of code. You can do this the same way you define complex functions.

The simplest control structure is the `if` statement. This statement *always* has to have an `else` clause. If you do not want an `else` clause, you can use the `-->` operator discussed earlier.

    x := if (a > b) (
           a * 100
         ) else (
           b * 100
         )

More complicated conditions could benefit from a `cond` statement. This is a list of conditions and statements; the statement corresponding to the first condition will be executed.

    cond [
      a > b -> "greater",
      a = b -> "equal",
      a < b -> "lesser"
    ]
      
If none of the conditions are true, it will return `false`.

You can also use a `switch` statement.

    switch x [
      1 -> "one",
      2 -> "two",
      3 -> "many"
    ]

This executes the statement matching the first value passed in (`x` in this case).

There are also several loops. You can use a simple `while` loop:

    while (x > 0) (
      x <- x - 10
      print x
    )

or a do-while loop:

    do (
      x <- x - 10
      print x
    ) while (x > 0)

You can also use a for loop to iterate over a list:

    for x in (1..10) (
      y := x^2 + 2*x + 10
      print ("(%d, %d)" % [x, y])
    )

### Useful Functions

There are also a bunch of fairly standard but useful functions for list manipulation like `map` and `fold`. They all work as expected.

## Objects

The one main feature still missing from the interpreter is support for objects.

I want to add objects that behave much like objects in JavaScript or Lua. I want them to be as simple and as flexible as possible; particularly, I will not add classes to the language and I will let programmers have full control of how values are set and looked up in objects (much like proxies do in JavaScript).

However, I actually want to go one step further than JavaScript or Lua: I want to unify the idea of scopes (environments) and objects. I want to be able to treat a function's environment as an object and to set an arbitrary object as a function's environment. This will give the programmer full control over how variables behave inside a function, making the `with` function for custom scoping redundant. You will be able to do anything you want (including introducing arbitrary variables) with a function, so you would be able to implement `with` in terms of objects and environments rather than needing it to be provided by the interpreter.
